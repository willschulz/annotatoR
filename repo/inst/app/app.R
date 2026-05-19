# =========================================================================
# annotatoR – Shiny annotation app
#
# Backend  : SQLite  (via annotatoR package helpers)
# Identity : signed cookie  +  optional Tailscale auto-login
# Audit    : every label write appends to label_events
# =========================================================================

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(fontawesome)
library(jsonlite)
library(DBI)
library(RSQLite)
library(pool)
library(dplyr)
library(digest)

# ---- Resolve paths set by annotator_run_app() ---------------------------
db_path            <- getOption("annotator.db_path")
cookie_secret_path <- getOption("annotator.cookie_secret_path")

if (is.null(db_path)) {
  # Walk upward from the app directory looking for a sibling state/ directory.
  # The canonical layout is:  annotatoR/repo/inst/app/app.R
  #                           annotatoR/state/annotatoR.sqlite
  # So from inst/app/ we need to go up until we find a dir that contains state/.
  env_db <- Sys.getenv("ANNOTATOR_DB", unset = "")
  if (nzchar(env_db) && file.exists(env_db)) {
    db_path <- normalizePath(env_db)
  } else {
    anchor <- getwd()
    for (i in 0:5) {
      candidate_dir <- normalizePath(
        do.call(file.path, as.list(c(anchor, rep("..", i), "state"))),
        mustWork = FALSE
      )
      candidate_db <- file.path(candidate_dir, "annotatoR.sqlite")
      if (file.exists(candidate_db)) {
        db_path <- candidate_db
        break
      }
    }
    if (is.null(db_path)) {
      # Absolute fallback for the standard layout
      db_path <- "/home/wschulz/projects/tools/annotatoR/state/annotatoR.sqlite"
    }
  }
  message("annotatoR: resolved db_path = ", db_path)
}
if (is.null(cookie_secret_path)) {
  cookie_secret_path <- file.path(dirname(db_path), ".cookie_secret")
}

# ---- Cookie secret -------------------------------------------------------
if (!file.exists(cookie_secret_path)) {
  dir.create(dirname(cookie_secret_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    paste0(sample(c(letters, LETTERS, 0:9), 64, replace = TRUE), collapse = ""),
    cookie_secret_path
  )
}
COOKIE_SECRET <- trimws(readLines(cookie_secret_path, n = 1, warn = FALSE))
COOKIE_NAME   <- "annotator_token"
COOKIE_MAX_AGE <- 60 * 60 * 24 * 90  # 90 days

# ---- Cookie helpers -------------------------------------------------------
make_token <- function(user_id) {
  expiry <- as.integer(Sys.time()) + COOKIE_MAX_AGE
  payload <- paste(user_id, expiry, sep = "|")
  sig <- digest::hmac(COOKIE_SECRET, payload, algo = "sha256")
  paste(payload, sig, sep = "|")
}

verify_token <- function(token) {
  parts <- strsplit(token, "\\|", fixed = FALSE)[[1]]
  if (length(parts) != 3) return(NULL)
  user_id <- parts[1]
  expiry  <- as.integer(parts[2])
  sig     <- parts[3]
  expected_sig <- digest::hmac(COOKIE_SECRET, paste(user_id, expiry, sep = "|"), algo = "sha256")
  if (!identical(sig, expected_sig)) return(NULL)
  if (as.integer(Sys.time()) > expiry) return(NULL)
  user_id
}

# ---- SQLite pool ----------------------------------------------------------
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

pool <- dbPool(
  drv      = RSQLite::SQLite(),
  dbname   = db_path,
  onCreate = function(con) {
    dbExecute(con, "PRAGMA journal_mode = WAL;")
    dbExecute(con, "PRAGMA busy_timeout = 5000;")
    dbExecute(con, "PRAGMA foreign_keys = ON;")
  }
)

onStop(function() {
  poolClose(pool)
})

# Ensure schema exists (idempotent)
local({
  con <- poolCheckout(pool)
  on.exit(poolReturn(con), add = TRUE)

  dbExecute(con, "
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
  dbExecute(con, "
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
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id      TEXT PRIMARY KEY,
      display_name TEXT,
      auth_source  TEXT DEFAULT 'manual',
      created_at   TEXT DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
    );
  ")
})

# ---- Data helpers ---------------------------------------------------------

get_eligible_data <- function(pool, annotator) {
  dbGetQuery(
    pool,
    "SELECT * FROM items
     WHERE annotator_id = ?
     ORDER BY
       CASE WHEN annotation_response IS NOT NULL THEN 0 ELSE 1 END,
       annotation_instruction,
       created_at",
    params = list(annotator)
  )
}

# Find positions where consecutive pending items switch instruction group.
# Returns a data.frame(boundary_pos, next_instruction); boundary_pos is the
# 1-indexed position of the last item in the current group (percentage on the
# progress bar = boundary_pos / nrow * 100). Only boundaries where BOTH
# neighbours are still pending (annotation_response IS NA) count, so the
# dense completed-items block does not produce spurious dividers.
compute_instruction_boundaries <- function(df) {
  n <- nrow(df)
  if (is.null(df) || n < 2) {
    return(data.frame(boundary_pos = integer(0),
                      next_instruction = character(0),
                      stringsAsFactors = FALSE))
  }
  instr   <- df$annotation_instruction
  pending <- is.na(df$annotation_response)
  i       <- seq_len(n - 1)
  hit     <- pending[i] & pending[i + 1L] & (instr[i] != instr[i + 1L])
  data.frame(
    boundary_pos     = i[hit],
    next_instruction = instr[i + 1L][hit],
    stringsAsFactors = FALSE
  )
}

# For the current index, return the active instruction and how many pending
# items remain in its contiguous pending run (forward + backward from idx).
current_group_status <- function(df, idx) {
  n <- nrow(df)
  if (is.null(df) || n == 0 || idx < 1 || idx > n) {
    return(list(current_instruction = NA_character_, n_remaining_in_group = 0L))
  }
  instr        <- df$annotation_instruction
  pending      <- is.na(df$annotation_response)
  current_inst <- instr[idx]

  # Walk back while same instruction (do not require pending for the walk,
  # because the user may be on an already-annotated item within a group).
  lo <- idx
  while (lo > 1L && identical(instr[lo - 1L], current_inst)) lo <- lo - 1L
  hi <- idx
  while (hi < n  && identical(instr[hi + 1L], current_inst)) hi <- hi + 1L

  remaining <- sum(pending[lo:hi])
  list(current_instruction  = current_inst,
       n_remaining_in_group = as.integer(remaining))
}

record_event <- function(pool, item_id, instr_hash, annotator_id,
                         field, old_value, new_value) {
  dbExecute(
    pool,
    "INSERT INTO label_events (item_id, instr_hash, annotator_id, field, old_value, new_value)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(item_id, instr_hash, annotator_id, field,
                  as.character(old_value), as.character(new_value))
  )
}

update_item <- function(pool, id, instruction_hash, annotator_id,
                        field, new_value) {
  old <- dbGetQuery(
    pool,
    sprintf("SELECT %s FROM items WHERE id = ? AND instruction_hash = ? AND annotator_id = ?", field),
    params = list(id, instruction_hash, annotator_id)
  )
  old_value <- if (nrow(old) > 0) as.character(old[[1]]) else NA_character_

  n <- dbExecute(
    pool,
    sprintf("UPDATE items SET %s = ?, updated_at = strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ','now')
             WHERE id = ? AND instruction_hash = ? AND annotator_id = ?", field),
    params = list(new_value, id, instruction_hash, annotator_id)
  )

  short_field <- sub("^annotation_", "", field)
  record_event(pool, id, instruction_hash, annotator_id,
               short_field, old_value, as.character(new_value))
  n
}

user_exists <- function(pool, user_id) {
  res <- dbGetQuery(pool, "SELECT COUNT(*) AS n FROM users WHERE user_id = ?",
                    params = list(user_id))
  res$n > 0
}

ensure_user <- function(pool, user_id, auth_source = "manual") {
  dbExecute(
    pool,
    "INSERT OR IGNORE INTO users (user_id, display_name, auth_source) VALUES (?, ?, ?)",
    params = list(user_id, user_id, auth_source)
  )
}

# ---- UI helper: NULL-safe value getter ------------------------------------
get_value <- function(x, default) {
  if (is.null(x) || length(x) == 0) default else x
}

# =========================================================================
# UI
# =========================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #dbdbdb;
        color: rgb(66, 66, 66);
      }
      #login_panel {
        width: 300px;
        max-width: 100%;
        margin: 0 auto;
        padding: 20px;
      }
      #snippet {
        background-color: #ffffff;
        border-color: #2e2e2e;
        padding: 5px;
        font-size: 1.25em;
        min-height: 160px;
        position: relative;
        left: 50%;
        right: 50%;
        width: 100vw;
        margin-left: -50vw;
        margin-right: -50vw;
      }
      #snippet.flagged {
        background-color: #fcba03;
      }
      #displayText {
        color: #4d4d4d;
        text-align: left;
      }
      #copy_btn_container {
        margin-top: 6px;
        margin-bottom: 4px;
        text-align: left;
      }
      .btn-copy {
        font-size: 0.85em;
        padding: 3px 10px;
        opacity: 0.75;
      }
      .container-fluid {
        padding-right: 5px;
        padding-left: 5px;
      }
      .mainpanel-container {
        display: flex;
        flex-direction: column;
        height: 100vh;
      }
      .mainpanel {
        flex-grow: 1;
        margin: auto;
        width: 100%;
      }
      .annotation-button {
        font-size: 1.5em;
        margin: 8px 15px;
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 10px;
        transition: all 0.3s ease;
        position: relative;
        aspect-ratio: 1;
        min-width: 60px;
        min-height: 60px;
        max-width: 180px;
        max-height: 180px;
        width: 18vw;
        height: 18vw;
        border: none;
        outline: none;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }
      .annotation-button i {
        font-size: 1.5em;
        transition: all 0.3s ease;
        margin-bottom: 4px;
      }
      .annotation-button span {
        font-size: 0.8em;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 100%;
        transition: all 0.3s ease;
        text-align: center;
      }
      @media (max-width: 768px) {
        .annotation-button {
          font-size: 1.2em;
          margin: 8px 10px;
        }
        .annotation-button i {
          font-size: 1.8em;
        }
        .annotation-button span {
          font-size: 0.7em;
        }
      }
      @media (max-width: 480px) {
        .annotation-button {
          font-size: 1em;
          margin: 8px 5px;
        }
        .annotation-button i {
          font-size: 2em;
        }
        .annotation-button span {
          font-size: 0.6em;
        }
      }
      .annotation-button.clicked {
        box-shadow: 0 0 0 4px #ffffff, 0 0 0 8px currentColor !important;
        transform: scale(1.1);
        z-index: 2;
      }
      .annotation-button:hover {
        transform: scale(1.05);
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
      }
      .annotation-button.clicked:hover {
        transform: scale(1.1);
        box-shadow: 0 0 0 4px #ffffff, 0 0 0 8px currentColor !important;
      }
      .annotation-button.reveal-mode:hover {
        transform: none !important;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2) !important;
      }
      .annotation-button.reveal-mode.clicked:hover {
        transform: scale(1.1) !important;
        box-shadow: 0 0 0 4px #ffffff, 0 0 0 8px currentColor !important;
      }
      #flagButton {
        border-radius: 50%;
        width: 100px !important;
        height: 100px !important;
        padding: 0;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        background-color: #fcba03;
        color: white;
        font-size: 1em;
        min-width: unset;
      }
      #flagButton.flagged {
        box-shadow: 0 0 0 3px #dbdbdb, 0 0 0 6px #fcba03;
      }
      .progress-bar {
        background-color: #453700;
      }
      .progress-wrapper {
        position: relative;
      }
      .progress-dividers {
        position: absolute;
        left: 0;
        right: 0;
        bottom: 0;
        height: 20px;
        pointer-events: none;
      }
      .progress-divider {
        position: absolute;
        top: 0;
        bottom: 0;
        width: 2px;
        background-color: rgba(255, 255, 255, 0.85);
        box-shadow: 0 0 2px rgba(0, 0, 0, 0.6);
      }
      .progress-group-badge {
        position: absolute;
        top: -1.6em;
        transform: translateX(-50%);
        font-size: 0.8em;
        padding: 1px 6px;
        border-radius: 8px;
        background-color: #453700;
        color: #fff;
        white-space: nowrap;
      }
      #progress_group {
        margin-top: auto;
      }
      #progress_group .btn {
        font-size: 1.75em;
      }
      .sidebar {
        position: fixed;
        top: 0;
        right: -400px;
        width: 400px;
        height: 100vh;
        background-color: white;
        box-shadow: -2px 0 5px rgba(0,0,0,0.2);
        transition: right 0.3s ease;
        z-index: 1000;
        padding: 20px;
        overflow-y: auto;
      }
      .sidebar.open {
        right: 0;
      }
      .sidebar-toggle {
        position: absolute;
        top: 0;
        right: 0;
        z-index: 1001;
        background-color: #666666;
        color: white;
        border: none;
        border-radius: 5px;
        padding: 5px 10px;
        cursor: pointer;
        font-size: 0.9em;
      }
      .sidebar-toggle:hover {
        background-color: #4d4d4d;
      }
      .instruction-section {
        margin-bottom: 20px;
      }
      .instruction-section h4 {
        margin-bottom: 10px;
      }
      .keyboard-shortcut {
        background-color: #f0f0f0;
        padding: 2px 6px;
        border-radius: 3px;
        font-family: monospace;
        margin: 0 2px;
      }
    ")),

    # -- Cookie JS: read on load, set on login, clear on logout -----------
    tags$script(HTML(sprintf("
      var COOKIE_NAME = '%s';

      function getCookie(name) {
        var v = document.cookie.match('(^|;) ?' + name + '=([^;]*)(;|$)');
        return v ? decodeURIComponent(v[2]) : null;
      }
      function setCookie(name, value, maxAge) {
        document.cookie = name + '=' + encodeURIComponent(value) +
          ';path=/;max-age=' + maxAge + ';SameSite=Lax';
      }
      function deleteCookie(name) {
        document.cookie = name + '=;path=/;max-age=0';
      }

      // On page load send existing cookie (if any) to the server
      $(document).on('shiny:connected', function() {
        var tok = getCookie(COOKIE_NAME);
        Shiny.setInputValue('_cookie_token', tok || '');
      });

      // Handler: server asks us to set a cookie
      Shiny.addCustomMessageHandler('set_cookie', function(msg) {
        setCookie(msg.name, msg.value, msg.maxAge);
      });

      // Enter key on login input triggers button
      $(document).ready(function() {
        $('#user_name').on('keypress', function(e) {
          if (e.which == 13) {
            e.preventDefault();
            e.stopPropagation();
            var val = $('#user_name').val();
            setTimeout(function() {
              if ($('#user_name').val() === val) $('#login_button').click();
            }, 200);
            return false;
          }
        });
      });
    ", COOKIE_NAME))),

    tags$script(HTML("
      $(document).on('click', '#copyButton', function(e) {
        // Read text directly from the DOM -- no server round-trip, so the
        // user-gesture context is preserved for navigator.clipboard.writeText().
        var snippet = document.getElementById('snippet');
        var text = snippet ? (snippet.innerText || snippet.textContent || '') : '';
        // #region agent log
        console.log('[eba2fe][H-A fix] copyButton clicked; textLen=' + text.trim().length + ' isSecureContext=' + window.isSecureContext);
        // #endregion
        navigator.clipboard.writeText(text.trim()).then(function() {
          // #region agent log
          console.log('[eba2fe][H-A fix] clipboard write SUCCEEDED');
          // #endregion
          var btn = document.getElementById('copyButton');
          if (btn) {
            var label = btn.querySelector('span') || btn;
            var orig = label.innerText;
            label.innerText = 'Copied!';
            setTimeout(function() { label.innerText = orig; }, 1500);
          }
        }).catch(function(err) {
          // #region agent log
          console.error('[eba2fe][H-A fix] clipboard write FAILED: ' + String(err));
          // #endregion
        });
      });
    "))
  ),

  # Single reactive output switches between login and annotation views
  uiOutput("app_ui")
)

# =========================================================================
# SERVER
# =========================================================================

server <- function(input, output, session) {

  user_auth <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)

  # ---- Helper: complete login -------------------------------------------
  do_login <- function(user_id, source = "manual") {
    ensure_user(pool, user_id, auth_source = source)
    user_auth(TRUE)
    user_info(list(user = user_id))

    # Set persistent cookie
    token <- make_token(user_id)
    session$sendCustomMessage("set_cookie", list(
      name   = COOKIE_NAME,
      value  = token,
      maxAge = COOKIE_MAX_AGE
    ))
  }

  # ---- Auto-login: Tailscale header or cookie ----------------------------
  observeEvent(input$`_cookie_token`, {
    if (user_auth()) return()  # already logged in

    # 1) Tailscale identity header (set by Tailscale Serve)
    ts_user <- session$request$HTTP_TAILSCALE_USER_LOGIN
    if (!is.null(ts_user) && nzchar(ts_user)) {
      message("annotatoR: auto-login via Tailscale header: ", ts_user)
      do_login(ts_user, source = "tailscale")
      return()
    }

    # 2) Signed cookie
    token <- input$`_cookie_token`
    if (!is.null(token) && nzchar(token)) {
      uid <- verify_token(token)
      if (!is.null(uid) && user_exists(pool, uid)) {
        message("annotatoR: auto-login via cookie: ", uid)
        do_login(uid, source = "manual")
        return()
      }
    }
  }, once = TRUE)

  # ---- Manual login button -----------------------------------------------
  observeEvent(input$login_button, {
    email <- trimws(input$user_name)
    if (!nzchar(email)) return()

    # Accept any email that has items assigned, OR that exists in the users table
    has_items <- dbGetQuery(
      pool,
      "SELECT COUNT(*) AS n FROM items WHERE annotator_id = ?",
      params = list(email)
    )$n > 0
    is_user <- user_exists(pool, email)

    if (has_items || is_user) {
      do_login(email)
    } else {
      showModal(modalDialog(
        title = "Unknown User",
        "No tasks have been assigned to this email. Please contact your administrator.",
        easyClose = TRUE
      ))
    }
  })

  logged_in_user <- reactive({
    req(user_auth())
    user_info()$user
  })

  # ---- Button creation helper -------------------------------------------
  create_annotation_buttons <- function(layout, labels, selected_label = NA,
                                         reveal_mode = FALSE) {
    button_list <- list()
    n_buttons <- length(labels$text)

    if (n_buttons > 2) {
      layout$gap <- "20px"
      layout$justifyContent <- "center"
      layout$flexWrap <- "wrap"
    }

    for (i in seq_len(n_buttons)) {
      btn <- layout$buttons[i, ]
      label_text <- labels$text[i]
      label_icon <- labels$icon[i]

      # Static indicator (empty label text)
      if (is.na(label_text) || label_text == "" || label_text == " ") {
        indicator_style <- sprintf("
          background-color: %s; color: %s; border-radius: %s;
          padding: %s; margin: %s; border: %s;
          display: flex; flex-direction: column; justify-content: center;
          align-items: center; gap: 4px;
          width: %s; height: %s; min-width: %s; min-height: %s;
          max-width: %s; max-height: %s;
          font-size: 5em; cursor: default; pointer-events: none;
        ",
          get_value(btn$backgroundColor, "transparent"),
          get_value(btn$color, "#000000"),
          get_value(btn$borderRadius, "0px"),
          get_value(btn$padding, "0px"),
          get_value(btn$margin, "8px 0px"),
          get_value(btn$border, "none"),
          get_value(btn$width, "auto"),
          get_value(btn$height, "auto"),
          get_value(btn$minWidth, "30px"),
          get_value(btn$minHeight, "30px"),
          get_value(btn$maxWidth, "50px"),
          get_value(btn$maxHeight, "50px")
        )
        button_list[[i]] <- div(
          style = "display: flex; flex-direction: column; align-items: center;",
          div(style = indicator_style, icon(label_icon))
        )
      } else {
        # Clickable / reveal button
        button_style <- sprintf("
          background-color: %s; color: %s; border-radius: 50%%;
          padding: 0; display: flex; flex-direction: column;
          justify-content: center; align-items: center; gap: 8px;
          %s %s
        ",
          get_value(btn$backgroundColor, "#ffffff"),
          get_value(btn$color, "#000000"),
          if (reveal_mode) "cursor: default;" else "",
          if (reveal_mode) "pointer-events: none;" else ""
        )

        is_selected <- !is.na(selected_label) && selected_label == label_text
        cls <- paste("annotation-button",
                     if (reveal_mode) "reveal-mode" else "",
                     if (is_selected) "clicked" else "")

        inner <- div(
          style = "display: flex; flex-direction: column; align-items: center; gap: 4px;",
          icon(label_icon),
          span(label_text)
        )

        if (reveal_mode) {
          button_list[[i]] <- div(
            style = "display: flex; flex-direction: column; align-items: center;",
            div(style = button_style, class = cls, inner)
          )
        } else {
          button_list[[i]] <- div(
            style = "display: flex; flex-direction: column; align-items: center;",
            actionButton(paste0("btn_", i), inner,
                         style = button_style, class = cls)
          )
        }
      }
    }
    button_list
  }

  # ---- Reactive UI: login form OR annotation interface --------------------
  output$app_ui <- renderUI({

    # Not logged in -> show login form
    if (!user_auth()) {
      return(div(id = "login_panel",
        div(
          style = "text-align: center; width: 300px; max-width: 100%; margin: 0 auto; padding: 20px;",
          textInput("user_name", "", placeholder = "you@example.com"),
          actionButton("login_button", "Login", class = "btn-primary")
        )
      ))
    }

    # Logged in -> fetch data and render annotation UI
    data <- get_eligible_data(pool, logged_in_user())

    if (nrow(data) == 0) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h2("No Data Available"),
        p("No annotation data has been assigned to you."),
        p(paste0("Logged in as: ", logged_in_user())),
        p("Please contact your administrator.")
      ))
    }

    tagList(
      # Sidebar
      div(id = "sidebar", class = "sidebar",
          h3("Annotation Instructions"),
          div(class = "instruction-section",
              h4("Navigation"),
              p("Use the arrow buttons or keyboard shortcuts:"),
              tags$ul(
                tags$li(span(class = "keyboard-shortcut", "\u2190"), " or ",
                        span(class = "keyboard-shortcut", "Back"), " to go back"),
                tags$li(span(class = "keyboard-shortcut", "\u2192"), " or ",
                        span(class = "keyboard-shortcut", "Next"), " to go forward")
              )
          ),
          div(class = "instruction-section",
              h4("Flagging"),
              p("Press ", span(class = "keyboard-shortcut", "F"),
                " to flag examples for later review.")
          ),
          div(class = "instruction-section",
              h4("Tips"),
              tags$ul(
                tags$li("You can review your decisions by navigating back and forth"),
                tags$li("The progress bar shows how many examples you've completed"),
                tags$li("Use the flag button to mark examples you want to revisit")
              )
          )
      ),

      div(class = "mainpanel-container",
          mainPanel(class = "mainpanel",
                    div(style = "position: relative;",
                        h2(htmlOutput("displayInstruction")),
                        actionButton("toggle_sidebar", "Instructions",
                                     class = "sidebar-toggle")
                    ),
                    div(id = "snippet", htmlOutput("displayText")),
                    div(id = "copy_btn_container",
                        actionButton("copyButton",
                                     label = tagList(icon("copy"), "Copy text"),
                                     class = "btn-copy")),
                    br(),
                    uiOutput("annotation_buttons"),
                    br()
          ),
          div(id = "progress_group",
              div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                  actionButton("backButton", "<"),
                  actionButton("flagButton", "Flag", icon = icon("flag"), width = "10%"),
                  actionButton("nextButton", ">")
              ),
              div(class = "progress-wrapper",
                  progressBar(id = "progress", display_pct = TRUE, value = 0, total = 100),
                  uiOutput("progress_dividers", class = "progress-dividers")
              )
          )
      )
    )
  })

  # ---- Server logic (post-login) ----------------------------------------
  observeEvent(logged_in_user(), {
    req(logged_in_user())
    data <- get_eligible_data(pool, logged_in_user())

    values <- reactiveValues(
      data = data,
      completion_modal_shown = FALSE,
      index = if (nrow(data) > 0) {
        unannotated <- which(is.na(data$annotation_response))
        if (length(unannotated) > 0) min(unannotated) else 1
      } else 1
    )

    req(values$data)

    # Show completion if everything is done on load
    if (nrow(data) > 0 &&
        all(!is.na(data$annotation_response)) &&
        !values$completion_modal_shown) {
      values$completion_modal_shown <- TRUE
      showModal(modalDialog(
        title = "Annotation Complete",
        paste("You have finished annotating all assigned content.",
              "You can still navigate through your annotations to review them."),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    }

    output$displayText <- renderText({
      req(values$data)
      values$data$annotation_html[values$index]
    })

    output$displayInstruction <- renderText({
      req(values$data)
      values$data$annotation_instruction[values$index]
    })

    # Dividers + current-group badge overlaid on the progress bar.
    # Reacts to values$index and values$data changes; dividers are only drawn
    # for instruction-group boundaries strictly ahead of the current index,
    # so the still-to-do portion of the bar is what gets subdivided.
    output$progress_dividers <- renderUI({
      req(values$data, nrow(values$data) > 0)
      total <- nrow(values$data)
      idx   <- values$index
      bnds  <- compute_instruction_boundaries(values$data)
      if (nrow(bnds) > 0) {
        bnds <- bnds[bnds$boundary_pos > idx, , drop = FALSE]
      }

      grp <- current_group_status(values$data, idx)

      badge_pos_pct <- if (nrow(bnds) > 0) {
        bnds$boundary_pos[1] / total * 100
      } else {
        100
      }

      divider_tags <- if (nrow(bnds) > 0) {
        lapply(bnds$boundary_pos, function(p) {
          div(class = "progress-divider",
              style = sprintf("left: %.3f%%;", p / total * 100))
        })
      } else {
        list()
      }

      badge_tag <- if (!is.null(grp$n_remaining_in_group) &&
                      grp$n_remaining_in_group > 0) {
        div(class = "progress-group-badge",
            title = if (!is.na(grp$current_instruction))
                      grp$current_instruction else "",
            style = sprintf("left: %.3f%%;", badge_pos_pct),
            sprintf("%d left", grp$n_remaining_in_group))
      } else {
        NULL
      }

      tagList(divider_tags, badge_tag)
    })

    # Render annotation buttons
    output$annotation_buttons <- renderUI({
      req(values$data)
      current <- values$data[values$index, ]
      tryCatch({
        layout <- fromJSON(current$button_layout)
        labels <- fromJSON(current$annotation_labels)
        div(
          style = sprintf(
            "display:%s; justify-content:%s; align-items:%s; gap:%s;",
            get_value(layout$display, "flex"),
            get_value(layout$justifyContent, "center"),
            get_value(layout$alignItems, "center"),
            get_value(layout$gap, "40px")
          ),
          create_annotation_buttons(
            layout, labels,
            current$annotation_response,
            as.logical(current$reveal_mode)
          )
        )
      }, error = function(e) {
        div(style = "color:red; padding:20px;",
            p("Error loading annotation interface."),
            p("Details: ", e$message))
      })
    })

    # ---- UI state observer -----------------------------------------------
    observeEvent(list(values$index, input$flagButton), {
      req(values$data, nrow(values$data) > 0, values$index <= nrow(values$data))
      updateProgressBar(session, "progress",
                        value = values$index, total = nrow(values$data))

      labels_json <- values$data$annotation_labels[values$index]
      req(labels_json, !is.na(labels_json))
      current_labels   <- fromJSON(labels_json)
      current_response <- values$data[values$index, "annotation_response"]

      for (i in seq_along(current_labels$text)) {
        btn_id <- paste0("btn_", i)
        should_click <- !is.na(current_response) &&
          current_response == current_labels$text[i]
        if (should_click) addClass(btn_id, "clicked")
        else              removeClass(btn_id, "clicked")
      }

      flagged <- as.numeric(values$data[values$index, "annotation_flagged"])
      if (!is.na(flagged) && flagged == 1) {
        addClass("flagButton", "flagged")
        addClass("snippet", "flagged")
      } else {
        removeClass("flagButton", "flagged")
        removeClass("snippet", "flagged")
      }
    }, ignoreInit = FALSE)

    # ---- Generic annotation handler --------------------------------------
    handle_annotation <- function(btn_index) {
      if (values$index > nrow(values$data)) return()

      # Ignore clicks in reveal mode
      rm_val <- as.logical(values$data[values$index, "reveal_mode"])
      if (!is.na(rm_val) && rm_val) return()

      current_labels <- fromJSON(values$data$annotation_labels[values$index])
      selected_label <- current_labels$text[btn_index]

      values$data[values$index, "annotation_response"] <- selected_label

      update_item(pool,
                  id               = values$data[values$index, "id"],
                  instruction_hash = values$data[values$index, "instruction_hash"],
                  annotator_id     = values$data[values$index, "annotator_id"],
                  field            = "annotation_response",
                  new_value        = selected_label)

      # Visual feedback
      addClass(paste0("btn_", btn_index), "clicked")
      for (j in setdiff(seq_along(current_labels$text), btn_index)) {
        removeClass(paste0("btn_", j), "clicked")
      }

      # Advance
      if (values$index < nrow(values$data)) {
        values$index <- values$index + 1
      }

      # Check completion
      if (all(!is.na(values$data$annotation_response)) &&
          !values$completion_modal_shown) {
        values$completion_modal_shown <- TRUE
        showModal(modalDialog(
          title = "Annotation Complete",
          paste("You have finished annotating all assigned content.",
                "You can still navigate through your annotations to review them."),
          easyClose = TRUE, footer = modalButton("Close")
        ))
      }
    }

    observeEvent(input$btn_1, handle_annotation(1))
    observeEvent(input$btn_2, handle_annotation(2))
    observeEvent(input$btn_3, handle_annotation(3))
    observeEvent(input$btn_4, handle_annotation(4))

    # ---- Flag handler ----------------------------------------------------
    observeEvent(input$flagButton, {
      if (values$index > nrow(values$data)) return()
      new_flag <- if (values$data[values$index, "annotation_flagged"] == 1) 0L else 1L
      values$data[values$index, "annotation_flagged"] <- new_flag
      update_item(pool,
                  id               = values$data[values$index, "id"],
                  instruction_hash = values$data[values$index, "instruction_hash"],
                  annotator_id     = values$data[values$index, "annotator_id"],
                  field            = "annotation_flagged",
                  new_value        = new_flag)
    })

    # ---- Navigation ------------------------------------------------------
    observeEvent(input$nextButton, {
      if (values$index < nrow(values$data)) values$index <- values$index + 1
    })
    observeEvent(input$backButton, {
      if (values$index > 1) values$index <- values$index - 1
    })

    # ---- Keyboard shortcuts -----------------------------------------------
    runjs("
      $(document).keyup(function(e) {
        if (e.key === '1') $('#btn_1').click();
        else if (e.key === '2') $('#btn_2').click();
        else if (e.key === '3') $('#btn_3').click();
        else if (e.key === '4') $('#btn_4').click();
        else if (e.key === 'f') $('#flagButton').click();
        else if (e.key === 'ArrowRight') $('#nextButton').click();
        else if (e.key === 'ArrowLeft')  $('#backButton').click();
      });
    ")
  })

  # ---- Sidebar toggle ----------------------------------------------------
  observeEvent(input$toggle_sidebar, {
    toggleClass("sidebar", "open")
  })
}

# =========================================================================
shinyApp(ui = ui, server = server)
