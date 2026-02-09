#' @title Task creation and batch processing
#' @description Functions to create annotation tasks and insert batches of items
#'   into the annotatoR SQLite database. These replace the former Python
#'   process_batch*.py scripts.
#' @import DBI jsonlite digest
#' @keywords internal

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Create a batch of annotation items
#'
#' Takes a data frame of items, a label-format name, and a vector of annotator
#' emails. Each item is inserted once per annotator. The format configuration
#' is read from `inst/label_formats/<format>.json`.
#'
#' @param df A data frame with at least columns `id`, `annotation_instruction`,
#'   and `annotation_html` (or `annotation_text` as an alias). An optional
#'   `annotation_response` column pre-fills answers (useful for reveal mode).
#' @param format Character. Name of a label format (without `.json` extension),
#'   e.g. `"human_ai"`, `"sentiment"`, `"relation_strength"`.
#' @param annotators Character vector of annotator emails.
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @param reveal_mode Logical. If `TRUE`, items are inserted in read-only
#'   reveal mode (default `FALSE`).
#' @param on_conflict What to do when a row with the same PK exists:
#'   `"skip"` (default) silently ignores duplicates, `"update"` overwrites
#'   html/labels/layout/response/flagged fields.
#' @return Invisibly, the number of rows inserted.
#' @export
annotator_create_batch <- function(df, format, annotators,
                                    db_path = NULL,
                                    reveal_mode = FALSE,
                                    on_conflict = c("skip", "update")) {
  on_conflict <- match.arg(on_conflict)
  stopifnot(is.data.frame(df))

  # Normalize column names: accept annotation_text as an alias

  if ("annotation_text" %in% names(df) && !"annotation_html" %in% names(df)) {
    names(df)[names(df) == "annotation_text"] <- "annotation_html"
  }

  required <- c("id", "annotation_instruction", "annotation_html")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Resolve and report the actual db path
  resolved_db <- annotator_db_path(db_path)
  message(sprintf("annotatoR: db_path resolved to: %s", resolved_db))
  message(sprintf("annotatoR: db file exists: %s", file.exists(resolved_db)))
  message(sprintf("annotatoR: input df has %d rows, %d unique ids", nrow(df), length(unique(df$id))))
  message(sprintf("annotatoR: annotators = [%s]", paste(annotators, collapse = ", ")))
  message(sprintf("annotatoR: format = '%s', on_conflict = '%s', reveal_mode = %s",
                  format, on_conflict, reveal_mode))

  # Load format config
  format_config <- load_label_format(format)
  labels_json <- jsonlite::toJSON(format_config$annotation_labels, auto_unbox = TRUE)
  layout_json <- jsonlite::toJSON(format_config$button_layout, auto_unbox = TRUE)
  message(sprintf("annotatoR: loaded format '%s' (labels: %s)", format, as.character(labels_json)))

  # Open connection
  con <- annotator_connect_plain(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Report pre-insert state
  pre_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM items")$n
  message(sprintf("annotatoR: items table has %d rows before insert", pre_count))

  # Ensure annotators exist
  for (a in annotators) {
    DBI::dbExecute(
      con,
      "INSERT OR IGNORE INTO users (user_id, display_name, auth_source) VALUES (?, ?, 'manual')",
      params = list(a, a)
    )
  }

  # Report registered users
  users <- DBI::dbGetQuery(con, "SELECT user_id, auth_source FROM users")
  message(sprintf("annotatoR: registered users: [%s]",
                  paste(sprintf("%s (%s)", users$user_id, users$auth_source), collapse = ", ")))

  n_inserted <- 0L
  n_skipped  <- 0L
  n_total    <- nrow(df) * length(annotators)

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    instr_hash <- digest::digest(row$annotation_instruction, algo = "md5", serialize = FALSE)

    # Pre-filled response (for reveal batches, etc.)
    response <- if ("annotation_response" %in% names(row) && !is.na(row$annotation_response)) {
      as.character(row$annotation_response)
    } else {
      NA_character_
    }

    flagged <- if ("annotation_flagged" %in% names(row) && !is.na(row$annotation_flagged)) {
      as.integer(row$annotation_flagged)
    } else {
      0L
    }

    for (a in annotators) {
      if (on_conflict == "skip") {
        sql <- "INSERT OR IGNORE INTO items
                (id, instruction_hash, annotator_id, annotation_instruction,
                 annotation_html, annotation_labels, button_layout,
                 annotation_response, annotation_flagged, reveal_mode)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      } else {
        sql <- "INSERT INTO items
                (id, instruction_hash, annotator_id, annotation_instruction,
                 annotation_html, annotation_labels, button_layout,
                 annotation_response, annotation_flagged, reveal_mode)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(id, instruction_hash, annotator_id) DO UPDATE SET
                  annotation_html    = excluded.annotation_html,
                  annotation_labels  = excluded.annotation_labels,
                  button_layout      = excluded.button_layout,
                  annotation_response = excluded.annotation_response,
                  annotation_flagged  = excluded.annotation_flagged,
                  reveal_mode         = excluded.reveal_mode,
                  updated_at          = strftime('%Y-%m-%dT%H:%M:%SZ','now')"
      }

      res <- DBI::dbExecute(con, sql, params = list(
        as.character(row$id),
        instr_hash,
        a,
        as.character(row$annotation_instruction),
        as.character(row$annotation_html),
        as.character(labels_json),
        as.character(layout_json),
        response,
        flagged,
        as.integer(reveal_mode)
      ))
      n_inserted <- n_inserted + res
      if (res == 0L) n_skipped <- n_skipped + 1L
    }
  }

  # Report post-insert state
  post_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM items")$n
  message(sprintf("annotatoR: items table has %d rows after insert (delta: %d)",
                  post_count, post_count - pre_count))
  message(sprintf("annotatoR: attempted %d inserts: %d inserted, %d skipped (on_conflict='%s')",
                  n_total, n_inserted, n_skipped, on_conflict))

  if (n_skipped > 0 && on_conflict == "skip") {
    message("annotatoR: NOTE -- skipped rows already existed (same id + instruction_hash + annotator_id).",
            " Use on_conflict='update' to overwrite existing rows.")
  }

  # Per-annotator item counts for verification
  for (a in annotators) {
    a_count <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n, SUM(annotation_response IS NULL) AS n_pending FROM items WHERE annotator_id = ?",
      params = list(a)
    )
    message(sprintf("annotatoR: annotator '%s' now has %d total items (%d pending)",
                    a, a_count$n, a_count$n_pending))
  }

  invisible(n_inserted)
}

# ---------------------------------------------------------------------------
# Format loading
# ---------------------------------------------------------------------------

#' Load a label format JSON file
#'
#' Searches for `<name>.json` in `inst/label_formats/` within the installed
#' package, or in a `label_formats/` directory relative to the working
#' directory for development use.
#'
#' @param name Format name (without `.json`).
#' @return Parsed list from the JSON file.
#' @keywords internal
load_label_format <- function(name) {
  # Installed package path
  pkg_path <- system.file("label_formats", paste0(name, ".json"), package = "annotatoR")
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    return(jsonlite::fromJSON(pkg_path))
  }

  # Development fallback: look relative to working directory
  dev_paths <- c(
    file.path("inst", "label_formats", paste0(name, ".json")),
    file.path("label_formats", paste0(name, ".json"))
  )
  for (p in dev_paths) {
    if (file.exists(p)) return(jsonlite::fromJSON(p))
  }

  stop("Label format '", name, "' not found. Searched:\n  ",
       paste(c(pkg_path, dev_paths), collapse = "\n  "))
}
