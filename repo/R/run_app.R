#' Launch the annotatoR Shiny application
#'
#' Starts the annotation web UI backed by the SQLite database.
#'
#' @param db_path Path to the SQLite file (default: auto-resolved via
#'   [annotator_db_path()]).
#' @param port Port number for the Shiny server (default 3839).
#' @param host Host to bind to. Use `"127.0.0.1"` for local-only (behind
#'   Tailscale Serve) or `"0.0.0.0"` for direct access (default `"127.0.0.1"`).
#' @param cookie_secret_path Path to the file containing the HMAC cookie
#'   secret. Auto-generated on first run if it doesn't exist
#'   (default: `state/.cookie_secret`).
#' @param launch.browser Logical. Open a browser window? (default `FALSE`
#'   for headless server use).
#' @return This function does not return; it runs the Shiny app.
#' @export
annotator_run_app <- function(db_path = NULL,
                               port = 3839L,
                               host = "127.0.0.1",
                               cookie_secret_path = NULL,
                               launch.browser = FALSE) {
  message("annotatoR: starting [centralization-smoke-test-260518]")
  db_path <- annotator_db_path(db_path)

  # Resolve cookie secret

  if (is.null(cookie_secret_path)) {
    cookie_secret_path <- file.path(dirname(db_path), ".cookie_secret")
  }
  if (!file.exists(cookie_secret_path)) {
    secret <- paste0(
      sample(c(letters, LETTERS, 0:9), 64, replace = TRUE),
      collapse = ""
    )
    writeLines(secret, cookie_secret_path)
    message("annotatoR: generated new cookie secret at ", cookie_secret_path)
  }

  # Set options so the app can pick them up
  options(
    annotator.db_path            = db_path,
    annotator.cookie_secret_path = cookie_secret_path
  )

  app_dir <- system.file("app", package = "annotatoR")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # Development fallback
    app_dir <- file.path(getwd(), "inst", "app")
  }

  shiny::runApp(
    appDir         = app_dir,
    port           = port,
    host           = host,
    launch.browser = launch.browser
  )
}
