make_export_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE items (
      id TEXT NOT NULL,
      instruction_hash TEXT NOT NULL,
      annotator_id TEXT NOT NULL,
      annotation_instruction TEXT NOT NULL,
      annotation_html TEXT,
      annotation_labels TEXT,
      button_layout TEXT,
      annotation_response TEXT,
      annotation_flagged INTEGER DEFAULT 0,
      reveal_mode INTEGER DEFAULT 0,
      created_at TEXT,
      updated_at TEXT,
      project TEXT,
      annotation_notes TEXT,
      PRIMARY KEY (id, instruction_hash, annotator_id)
    )
  ")

  rows <- data.frame(
    id = c("ers_1", "ers_2", "cap-topic-1", "fp_1"),
    instruction_hash = c("pair", "pair", "topic", "fp"),
    annotator_id = c("perry", "other", "other", "will"),
    annotation_instruction = "instruction",
    annotation_response = c("left", NA, "3", "yes"),
    created_at = c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04"),
    project = c(
      "Elite Rhetoric Scaling",
      "Elite Rhetoric Scaling",
      "CAP Tweet Topic Validation",
      "False Polarization"
    ),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "items", rows, append = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE label_events (
      event_id INTEGER PRIMARY KEY AUTOINCREMENT,
      item_id TEXT NOT NULL,
      instr_hash TEXT NOT NULL,
      annotator_id TEXT NOT NULL,
      field TEXT NOT NULL,
      old_value TEXT,
      new_value TEXT,
      ts TEXT
    )
  ")
  path
}

test_that("project filters select one or several projects", {
  db <- make_export_db()
  on.exit(unlink(db), add = TRUE)

  ers <- annotator_export(db, projects = "Elite Rhetoric Scaling")
  expect_equal(nrow(ers), 2L)
  expect_setequal(unique(ers$project), "Elite Rhetoric Scaling")

  selected <- annotator_export(
    db,
    projects = c("Elite Rhetoric Scaling", "CAP Tweet Topic Validation")
  )
  expect_equal(nrow(selected), 3L)
  expect_setequal(
    unique(selected$project),
    c("Elite Rhetoric Scaling", "CAP Tweet Topic Validation")
  )
})

test_that("project filtering composes with existing filters", {
  db <- make_export_db()
  on.exit(unlink(db), add = TRUE)

  result <- annotator_export(
    db,
    projects = "Elite Rhetoric Scaling",
    instruction_hash = "pair",
    annotators = "perry",
    completed_only = TRUE
  )

  expect_equal(result$id, "ers_1")
  expect_equal(result$annotation_response, "left")
})

test_that("invalid project filters fail closed", {
  db <- make_export_db()
  on.exit(unlink(db), add = TRUE)

  expect_error(
    annotator_export(db, projects = character()),
    "non-empty character vector"
  )
  expect_error(annotator_export(db, projects = ""), "non-empty character vector")
  expect_error(annotator_export(db, projects = NA_character_), "non-empty character vector")
  expect_error(annotator_export(db, projects = 1), "non-empty character vector")
})

test_that("export connections reject writes", {
  db <- make_export_db()
  on.exit(unlink(db), add = TRUE)

  con <- annotatoR:::annotator_connect_readonly(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_error(
    DBI::dbExecute(con, "DELETE FROM items"),
    "[Rr]eadonly|read-only"
  )

  expect_equal(nrow(annotator_export(db)), 4L)
})

test_that("CSV helper writes only the selected project", {
  db <- make_export_db()
  on.exit(unlink(db), add = TRUE)
  output_dir <- tempfile()
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  output <- annotator_export_csv(
    db_path = db,
    projects = "CAP Tweet Topic Validation",
    completed_only = TRUE,
    exports_dir = output_dir,
    prefix = "cap"
  )

  expect_true(file.exists(output))
  exported <- utils::read.csv(output)
  expect_equal(nrow(exported), 1L)
  expect_equal(exported$project, "CAP Tweet Topic Validation")
  expect_equal(exported$id, "cap-topic-1")
})
