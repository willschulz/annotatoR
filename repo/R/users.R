#' @title User management helpers
#' @description Functions to add, list, and look up annotator users.
#' @import DBI
#' @keywords internal

#' Register a new annotator user
#'
#' Inserts a user into the `users` table. If the user already exists the call
#' is a silent no-op.
#'
#' @param user_id Email address (primary key).
#' @param display_name Optional human-readable name.
#' @param auth_source One of `"manual"`, `"tailscale"`, or `"cloudflare"`.
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @return Invisibly, the number of rows inserted (0 if user existed).
#' @export
annotator_add_user <- function(user_id, display_name = user_id,
                                auth_source = "manual", db_path = NULL) {
  con <- annotator_connect_plain(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n <- DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO users (user_id, display_name, auth_source) VALUES (?, ?, ?)",
    params = list(user_id, display_name, auth_source)
  )
  if (n > 0) message("annotatoR: added user ", user_id)
  invisible(n)
}

#' List all registered users
#'
#' @param db_path Path to the SQLite file (default: auto-resolved).
#' @return A data frame with columns `user_id`, `display_name`, `auth_source`,
#'   `created_at`.
#' @keywords internal
annotator_list_users <- function(db_path = NULL) {
  con <- annotator_connect_plain(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM users ORDER BY created_at")
}

#' Check whether a user exists
#'
#' @param user_id Email address.
#' @param pool A pool or DBI connection.
#' @return Logical scalar.
#' @keywords internal
user_exists <- function(user_id, pool) {
  res <- DBI::dbGetQuery(
    pool,
    "SELECT COUNT(*) AS n FROM users WHERE user_id = ?",
    params = list(user_id)
  )
  res$n > 0
}

#' Ensure a user exists, creating them if necessary
#'
#' Used by the Shiny app to auto-register trusted-proxy users on first visit.
#'
#' @param user_id Email address.
#' @param auth_source One of `"manual"`, `"tailscale"`, or `"cloudflare"`.
#' @param pool A pool or DBI connection.
#' @return Invisibly, the user_id.
#' @keywords internal
ensure_user <- function(user_id, auth_source = "manual", pool) {
  DBI::dbExecute(
    pool,
    "INSERT OR IGNORE INTO users (user_id, display_name, auth_source) VALUES (?, ?, ?)",
    params = list(user_id, user_id, auth_source)
  )
  invisible(user_id)
}

#' Parse an exact Cloudflare Access email-to-annotator mapping
#'
#' The mapping is a comma-separated list of `access_email=annotator_id` pairs.
#' Access emails are normalized to lowercase; annotator IDs are preserved.
#'
#' @param specification Mapping string.
#' @return A named character vector keyed by normalized Access email.
#' @keywords internal
parse_cloudflare_identity_map <- function(specification) {
  if (length(specification) != 1L || is.na(specification) ||
      !nzchar(trimws(specification))) {
    stop("Cloudflare identity map must be a non-empty scalar string")
  }

  entries <- strsplit(specification, ",", fixed = TRUE)[[1L]]
  pairs <- lapply(entries, function(entry) {
    fields <- strsplit(entry, "=", fixed = TRUE)[[1L]]
    if (length(fields) != 2L) {
      stop("Each Cloudflare identity entry must contain exactly one '='")
    }
    access_email <- tolower(trimws(fields[[1L]]))
    annotator_id <- trimws(fields[[2L]])
    if (!nzchar(access_email) || !nzchar(annotator_id) ||
        !grepl("^[^@[:space:]]+@[^@[:space:]]+$", access_email)) {
      stop("Cloudflare identity entries require a valid email and annotator ID")
    }
    c(access_email = access_email, annotator_id = annotator_id)
  })

  access_emails <- vapply(pairs, `[[`, character(1), "access_email")
  if (anyDuplicated(access_emails)) {
    stop("Cloudflare identity map contains duplicate Access emails")
  }
  annotator_ids <- vapply(pairs, `[[`, character(1), "annotator_id")
  stats::setNames(annotator_ids, access_emails)
}

#' Resolve a verified Cloudflare Access email to an annotator ID
#'
#' @param access_email Value of `Cf-Access-Authenticated-User-Email`.
#' @param specification Mapping accepted by [parse_cloudflare_identity_map()].
#' @return The mapped annotator ID, or `NULL` when the email is absent or not
#'   allowlisted.
#' @keywords internal
resolve_cloudflare_identity <- function(access_email, specification) {
  if (is.null(access_email) || length(access_email) != 1L ||
      is.na(access_email) || !nzchar(trimws(access_email))) {
    return(NULL)
  }
  mapping <- parse_cloudflare_identity_map(specification)
  resolved <- unname(mapping[tolower(trimws(access_email))])
  if (length(resolved) != 1L || is.na(resolved) || !nzchar(resolved)) {
    return(NULL)
  }
  resolved
}
