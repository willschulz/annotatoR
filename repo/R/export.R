# Export helpers intentionally use a read-only SQLite connection. Keep all
# mutating package operations on annotator_connect_plain().

#' Export annotation labels
#'
#' Returns a tidy data frame of annotation items, optionally filtered by
#' project, instruction hash, annotator, or completion status. The database is
#' always opened read-only.
#'
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @param projects Optional character vector of project names to include.
#'   `NULL` means all projects.
#' @param instruction_hash Optional character vector of instruction hashes to
#'   include. `NULL` means all.
#' @param annotators Optional character vector of annotator emails to include.
#'   `NULL` means all.
#' @param completed_only Logical. If `TRUE`, only items with a non-NULL
#'   response are included (default `FALSE`).
#' @return A tibble.
#' @export
annotator_export <- function(db_path = NULL,
                              projects = NULL,
                              instruction_hash = NULL,
                              annotators = NULL,
                              completed_only = FALSE) {
  if (!is.null(projects)) {
    if (!is.character(projects) || length(projects) == 0L ||
        anyNA(projects) || any(!nzchar(trimws(projects)))) {
      stop(
        "`projects` must be NULL or a non-empty character vector of project names.",
        call. = FALSE
      )
    }
  }

  con <- annotator_connect_readonly(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "SELECT * FROM items WHERE 1=1"
  params <- list()

  if (!is.null(projects)) {
    placeholders <- paste(rep("?", length(projects)), collapse = ", ")
    query <- paste0(query, " AND project IN (", placeholders, ")")
    params <- c(params, as.list(projects))
  }

  if (!is.null(instruction_hash)) {
    placeholders <- paste(rep("?", length(instruction_hash)), collapse = ", ")
    query <- paste0(query, " AND instruction_hash IN (", placeholders, ")")
    params <- c(params, as.list(instruction_hash))
  }

  if (!is.null(annotators)) {
    placeholders <- paste(rep("?", length(annotators)), collapse = ", ")
    query <- paste0(query, " AND annotator_id IN (", placeholders, ")")
    params <- c(params, as.list(annotators))
  }

  if (completed_only) {
    query <- paste0(query, " AND annotation_response IS NOT NULL")
  }

  query <- paste0(query, " ORDER BY annotator_id, created_at")

  result <- if (length(params) > 0L) {
    DBI::dbGetQuery(con, query, params = params)
  } else {
    DBI::dbGetQuery(con, query)
  }
  dplyr::as_tibble(result)
}

#' Export the audit trail
#'
#' Returns the full `label_events` table as a tidy data frame.
#'
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @param since Optional ISO-8601 timestamp string. Only events after this
#'   time are returned.
#' @return A tibble.
#' @keywords internal
annotator_export_events <- function(db_path = NULL, since = NULL) {
  con <- annotator_connect_readonly(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!is.null(since)) {
    result <- DBI::dbGetQuery(
      con,
      "SELECT * FROM label_events WHERE ts >= ? ORDER BY ts",
      params = list(since)
    )
  } else {
    result <- DBI::dbGetQuery(con, "SELECT * FROM label_events ORDER BY ts")
  }
  dplyr::as_tibble(result)
}

#' Write an export to CSV in the exports/ directory
#'
#' Convenience wrapper around [annotator_export()] that writes the result to
#' a timestamped CSV in the `exports/` directory.
#'
#' @inheritParams annotator_export
#' @param exports_dir Path to the exports directory (default: auto-resolved).
#' @param prefix Filename prefix (default `"labels"`).
#' @param ... Additional filtering arguments passed to [annotator_export()].
#' @return Invisibly, the path to the written CSV file.
#' @export
annotator_export_csv <- function(db_path = NULL,
                                  exports_dir = NULL,
                                  prefix = "labels",
                                  ...) {
  if (is.null(exports_dir)) {
    exports_dir <- file.path(dirname(annotator_db_path(db_path)), "..", "exports")
  }
  dir.create(exports_dir, recursive = TRUE, showWarnings = FALSE)

  df <- annotator_export(db_path = db_path, ...)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fname <- file.path(exports_dir, sprintf("%s_%s.csv", prefix, ts))
  utils::write.csv(df, fname, row.names = FALSE)
  message("annotatoR: exported ", nrow(df), " rows to ", fname)
  invisible(fname)
}
