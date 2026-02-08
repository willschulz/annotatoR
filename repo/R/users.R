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
#' @param auth_source One of `"manual"` or `"tailscale"`.
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
#' Used by the Shiny app to auto-register Tailscale users on first visit.
#'
#' @param user_id Email address.
#' @param auth_source One of `"manual"` or `"tailscale"`.
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
