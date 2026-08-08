#' @title SQLite database helpers for annotatoR
#' @description Connection management, schema bootstrap, and WAL/busy-timeout
#'   configuration for the annotatoR SQLite backend.
#' @import DBI RSQLite pool
#' @keywords internal

# ---------------------------------------------------------------------------
# Default DB path (can be overridden by env var or function argument)
# ---------------------------------------------------------------------------

#' Resolve the database path
#'
#' Order of precedence:
#' 1. Explicit `db_path` argument
#' 2. `ANNOTATOR_DB` environment variable
#' 3. `<package-root>/../../state/annotatoR.sqlite` (the standard layout)
#' @param db_path Optional explicit path to the SQLite file.
#' @return Character scalar – resolved path.
#' @export
annotator_db_path <- function(db_path = NULL) {
  if (!is.null(db_path)) return(db_path)
  env <- Sys.getenv("ANNOTATOR_DB", unset = "")
  if (nzchar(env)) return(env)
  # Fallback: assume we are in the standard tools/annotatoR layout
  file.path(find_state_dir(), "annotatoR.sqlite")
}

# ---------------------------------------------------------------------------
# Connection pool
# ---------------------------------------------------------------------------

#' Open a pooled SQLite connection
#'
#' Creates the database file and all tables if they do not exist. Enables
#' WAL journal mode and a 5-second busy timeout.
#'
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @return A `pool::Pool` object.
#' @export
annotator_connect <- function(db_path = NULL) {
  db_path <- annotator_db_path(db_path)

  # Ensure the parent directory exists
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  p <- pool::dbPool(
    drv      = RSQLite::SQLite(),
    dbname   = db_path,
    onCreate = function(con) {
      # WAL mode for concurrent reads during Shiny use
      DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
      DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
      DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
      ensure_schema(con)
    }
  )
  p
}

#' Open a plain (non-pooled) SQLite connection
#'
#' Useful for one-off scripts and migration work. Sets the same PRAGMAs as
#' the pooled variant.
#'
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @return A `DBIConnection`.
#' @keywords internal
annotator_connect_plain <- function(db_path = NULL) {
  db_path <- annotator_db_path(db_path)
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  ensure_schema(con)
  con
}

#' Open a read-only SQLite connection
#'
#' Used by retrieval helpers that must never initialize, migrate, or otherwise
#' modify the annotation database.
#'
#' @param db_path Path to an existing SQLite file.
#' @return A read-only `DBIConnection`.
#' @keywords internal
annotator_connect_readonly <- function(db_path = NULL) {
  db_path <- annotator_db_path(db_path)
  if (!file.exists(db_path)) {
    stop("Annotation database does not exist: ", db_path, call. = FALSE)
  }

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = db_path,
    flags = RSQLite::SQLITE_RO
  )
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  DBI::dbExecute(con, "PRAGMA query_only = ON;")
  con
}

# ---------------------------------------------------------------------------
# Schema
# ---------------------------------------------------------------------------

#' Create tables if they don't already exist
#' @param con A DBI connection.
#' @keywords internal
ensure_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS items (
      id                     TEXT    NOT NULL,
      instruction_hash       TEXT    NOT NULL,
      annotator_id           TEXT    NOT NULL,
      annotation_instruction TEXT    NOT NULL,
      annotation_html        TEXT,
      annotation_labels      TEXT,
      button_layout          TEXT,
      annotation_response    TEXT    DEFAULT NULL,
      annotation_flagged     INTEGER DEFAULT 0,
      reveal_mode            INTEGER DEFAULT 0,
      created_at             TEXT    DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
      updated_at             TEXT    DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
      PRIMARY KEY (id, instruction_hash, annotator_id)
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS label_events (
      event_id     INTEGER PRIMARY KEY AUTOINCREMENT,
      item_id      TEXT    NOT NULL,
      instr_hash   TEXT    NOT NULL,
      annotator_id TEXT    NOT NULL,
      field        TEXT    NOT NULL,
      old_value    TEXT,
      new_value    TEXT,
      ts           TEXT    DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id      TEXT PRIMARY KEY,
      display_name TEXT,
      auth_source  TEXT DEFAULT 'manual',
      created_at   TEXT DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
    );
  ")

  # Add project column if not present (migration for existing DBs).
  # SQLite has no ALTER TABLE ADD COLUMN IF NOT EXISTS, so we suppress the
  # "duplicate column" error.
  tryCatch(
    DBI::dbExecute(con, "ALTER TABLE items ADD COLUMN project TEXT"),
    error = function(e) invisible(NULL)
  )
  # Backfill existing rows that pre-date the project column.
  DBI::dbExecute(con,
    "UPDATE items SET project = 'False Polarization' WHERE project IS NULL")

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Find the state/ directory relative to the package install location
#' @keywords internal
find_state_dir <- function() {
  # When running as an installed package the inst/ contents live at

  # system.file(..., package = "annotatoR"). The standard layout puts
  # state/ two levels above inst/ (tools/annotatoR/state).
  pkg_root <- system.file(package = "annotatoR")
  if (nzchar(pkg_root)) {
    candidate <- file.path(dirname(dirname(pkg_root)), "state")
    if (dir.exists(candidate)) return(candidate)
  }
  # Development fallback: walk up from the repo/ directory
  dev_candidate <- normalizePath(
    file.path(getwd(), "..", "state"),
    mustWork = FALSE
  )
  if (dir.exists(dev_candidate)) return(dev_candidate)
  # Last resort
  file.path(getwd(), "state")
}

#' Record a label event in the audit trail
#'
#' @param pool A pool or DBI connection.
#' @param item_id Item ID.
#' @param instr_hash Instruction hash.
#' @param annotator_id Annotator email.
#' @param field Which field changed ("response" or "flagged").
#' @param old_value Previous value (character or NULL).
#' @param new_value New value (character).
#' @keywords internal
record_label_event <- function(pool, item_id, instr_hash, annotator_id,
                                field, old_value, new_value) {
  DBI::dbExecute(
    pool,
    "INSERT INTO label_events (item_id, instr_hash, annotator_id, field, old_value, new_value)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(item_id, instr_hash, annotator_id, field,
                  as.character(old_value), as.character(new_value))
  )
}

#' Update an item field and record the change in the audit trail
#'
#' @param pool A pool or DBI connection.
#' @param id Item ID.
#' @param instruction_hash Instruction hash.
#' @param annotator_id Annotator email.
#' @param field Column name to update (annotation_response or annotation_flagged).
#' @param new_value New value.
#' @return Number of rows affected.
#' @keywords internal
update_item_field <- function(pool, id, instruction_hash, annotator_id,
                               field, new_value) {
  stopifnot(field %in% c("annotation_response", "annotation_flagged"))

  # Fetch old value
  old <- DBI::dbGetQuery(
    pool,
    sprintf("SELECT %s FROM items WHERE id = ? AND instruction_hash = ? AND annotator_id = ?", field),
    params = list(id, instruction_hash, annotator_id)
  )
  old_value <- if (nrow(old) > 0) as.character(old[[1]]) else NA_character_

  # Perform update

  n <- DBI::dbExecute(
    pool,
    sprintf("UPDATE items SET %s = ?, updated_at = strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ','now')
             WHERE id = ? AND instruction_hash = ? AND annotator_id = ?", field),
    params = list(new_value, id, instruction_hash, annotator_id)
  )

  # Audit trail
  short_field <- sub("^annotation_", "", field)
  record_label_event(pool, id, instruction_hash, annotator_id,
                     short_field, old_value, as.character(new_value))
  n
}
