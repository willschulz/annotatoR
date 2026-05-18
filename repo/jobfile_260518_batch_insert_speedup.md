# Jobfile: annotatoR `annotator_create_batch` insert speedup

**Created:** 2026-05-18
**Effort:** Small (1-2 hour change + test)
**Risk:** Low (same logic, same SQL, different transaction scope)

---

## Problem

`annotator_create_batch()` in `R/tasks.R` inserts rows one at a time with no
explicit transaction. SQLite in WAL mode wraps every un-transacted
`DBI::dbExecute()` call in its own implicit transaction, which means every
single row insert triggers a WAL record write + fsync to disk.

Observed rate: **~12 rows/sec** (10,937 rows / ~15 min) during the 2026-05-18
wave-6 push. Expected rate with a single explicit transaction: **500-1200
rows/sec** — a 50-100× speedup for the same inserts.

### Root cause (line-level)

`R/tasks.R` lines ~100-120 (approximately):

```r
for (i in seq_len(nrow(df))) {
  row <- df[i, , drop = FALSE]
  instr_hash <- digest::digest(row$annotation_instruction, ...)   # ← called n times, same string

  for (a in annotators) {
    res <- DBI::dbExecute(con, sql, params = list(...))  # ← one fsync per row
    n_inserted <- n_inserted + res
  }
}
```

Two compounding inefficiencies:
1. **No explicit transaction** — one fsync per row (the big one; ~95% of the
   wall time).
2. **`digest()` called per row** — for the `annotator_create_batch` call
   pattern used by the push QMD, all rows for a given issue share the same
   `annotation_instruction`, so `instr_hash` is recomputed ~1,000 times for
   the same string. Minor overhead compared to (1), but trivially fixable.

---

## Fix

### Fix 1 — Wrap the insert loop in a single explicit transaction (2-line change, 95% of the gain)

```r
# In annotator_create_batch(), replace:
n_inserted <- 0L
n_skipped  <- 0L
for (i in seq_len(nrow(df))) {
  ...
}

# With:
n_inserted <- 0L
n_skipped  <- 0L
DBI::dbWithTransaction(con, {
  for (i in seq_len(nrow(df))) {
    ...
  }
})
```

`DBI::dbWithTransaction()` wraps the entire block in `BEGIN TRANSACTION` /
`COMMIT`, collapsing 10,937 implicit transactions into 1 fsync. No other
logic changes. Rollback on error is also improved (currently a mid-loop error
leaves the table in a partially-written state; inside a transaction the whole
batch rolls back atomically).

### Fix 2 — Pre-compute `instr_hash` outside the row loop (trivial)

```r
# Before the loop, compute once per call (not once per row):
instr_hash <- digest::digest(df$annotation_instruction[1], algo = "md5", serialize = FALSE)
# NOTE: this assumes all rows share the same instruction (true for the push QMD).
# If rows can have different instructions, use:
# instr_hashes <- vapply(df$annotation_instruction, digest::digest,
#                        FUN.VALUE = character(1), algo = "md5", serialize = FALSE)
# Then index: instr_hash <- instr_hashes[i] inside the loop.
```

The current call pattern (one `annotator_create_batch()` call per issue, all
rows sharing the same instruction) means Fix 2 saves `nrow(df)` redundant MD5
computations. For a 1,200-row issue that's ~1,200 × ~50µs = ~60ms — negligible
compared to the transaction fix, but clean.

### Fix 3 — Optional bulk insert path for the "skip" conflict strategy

For the common `on_conflict = "skip"` case, the entire loop can be replaced
with a single parameterized `dbAppendTable`-style bulk insert:

```r
# Build the full rows tibble up front
rows_tbl <- tidyr::crossing(df, annotator_id = annotators) %>%
  dplyr::mutate(
    instruction_hash    = instr_hashes[match(annotation_instruction, unique(annotation_instruction))],
    annotation_labels   = as.character(labels_json),
    button_layout       = as.character(layout_json),
    annotation_response = NA_character_,
    annotation_flagged  = 0L,
    reveal_mode         = as.integer(reveal_mode)
  ) %>%
  dplyr::select(id, instruction_hash, annotator_id, annotation_instruction,
                annotation_html, annotation_labels, button_layout,
                annotation_response, annotation_flagged, reveal_mode)

# Use INSERT OR IGNORE via a temp table + INSERT SELECT
# (DBI::dbWriteTable with append=TRUE doesn't support OR IGNORE natively)
DBI::dbWriteTable(con, "items_tmp", rows_tbl, temporary = TRUE, overwrite = TRUE)
n_inserted <- DBI::dbExecute(con,
  "INSERT OR IGNORE INTO items SELECT * FROM items_tmp")
DBI::dbExecute(con, "DROP TABLE IF EXISTS items_tmp")
```

Fix 3 further reduces R-level overhead (the `for` loop itself) and allows
SQLite's bulk-insert path. Expected additional gain: 2-5× over Fix 1 alone
(from ~300 rows/sec → ~1000-2000 rows/sec). Worth doing but not necessary if
Fix 1 + 2 already bring inserts to <60 seconds.

---

## Implementation plan

1. **Edit `R/tasks.R`**: apply Fix 1 (transaction wrapper) and Fix 2
   (pre-compute `instr_hash`). Optionally apply Fix 3 for the `skip` path.
2. **Re-install** the package on the datascience VM:
   ```bash
   cd ~/projects/tools/annotatoR/repo
   Rscript -e "pak::local_install('.')"
   ```
   or equivalently:
   ```bash
   R CMD INSTALL .
   ```
3. **Benchmark**: run a test insert of ~1,000 rows with `on_conflict = "skip"`
   against a copy of the DB and confirm wall time drops from ~80 sec to <2 sec.
4. **Smoke test with the seed QMD**: do a dry-run render of
   `260423_wave06_sample.qmd` (no DB changes) to confirm no regressions.
5. **Update `RUNBOOK.md`** to note the transaction semantics and the expected
   insert rate.

---

## Files to edit

| File | Change |
|------|--------|
| `R/tasks.R` | Wrap inner loop in `DBI::dbWithTransaction()`; pre-compute `instr_hash` |
| `RUNBOOK.md` | Add note on transaction semantics and expected throughput |

No schema changes. No changes to `R/db.R`, `R/export.R`, or the Shiny app.

---

## Expected outcome

| Scenario | Before | After (Fix 1+2) |
|----------|--------|-----------------|
| 10,937-row push (1 annotator) | ~15 min | ~15-30 sec |
| 1,200-row issue batch | ~100 sec | ~1-2 sec |
| Error mid-batch | partial write (hard to clean up) | full rollback (atomic) |

The Shiny app is unaffected — `annotator_create_batch()` is a CLI/script
function and is not called from the reactive context.

---

## Deferred

- **Fix 3** (bulk insert path): implement after Fix 1+2 are confirmed working,
  if further speedup is needed.
- **Full AnnotatoR rewrite** (non-Shiny): long-term. Not required for this fix.
