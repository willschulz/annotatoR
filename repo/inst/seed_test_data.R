#!/usr/bin/env Rscript
# -----------------------------------------------------------
# Seed the SQLite database with a small test batch for wschulz
#
# Usage:
#   cd ~/projects/tools/annotatoR/repo
#   Rscript inst/seed_test_data.R
# -----------------------------------------------------------

# Use the package if installed, otherwise source db.R directly
tryCatch(library(annotatoR), error = function(e) {
  # Dev fallback: source the package R files directly
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
})

library(DBI)
library(RSQLite)
library(jsonlite)
library(digest)

db_path <- Sys.getenv("ANNOTATOR_DB", unset = "")
if (!nzchar(db_path)) {
  db_path <- normalizePath(
    file.path("..", "state", "annotatoR.sqlite"),
    mustWork = FALSE
  )
}

cat("Database path:", db_path, "\n")
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
dbExecute(con, "PRAGMA journal_mode = WAL;")
dbExecute(con, "PRAGMA busy_timeout = 5000;")

# --- Create schema --------------------------------------------------------
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

# --- Seed user ------------------------------------------------------------
dbExecute(con,
  "INSERT OR IGNORE INTO users (user_id, display_name, auth_source)
   VALUES ('wschulz', 'Will Schulz', 'manual')"
)

# --- Load format configs --------------------------------------------------
fmt_dir <- file.path("inst", "label_formats")
if (!dir.exists(fmt_dir)) fmt_dir <- file.path("label_formats")

human_ai  <- fromJSON(file.path(fmt_dir, "human_ai.json"))
sentiment <- fromJSON(file.path(fmt_dir, "sentiment.json"))

# --- Test items: human_ai format -----------------------------------------
hai_items <- data.frame(
  id = paste0("test_hai_", 1:5),
  instruction = rep("Classify if this response was written by a human or AI", 5),
  html = c(
    "<p>I went to the store yesterday and grabbed some milk. The cashier was really friendly and we chatted about the weather for a bit.</p>",
    "<p>Based on the comprehensive analysis of the provided dataset, it can be conclusively determined that the implementation of the proposed methodology yields statistically optimal results within the pre-specified parameters of the experimental framework.</p>",
    "<p>lol this is such a weird take. like, have you even watched the show?? the whole point is that the villain is supposed to be sympathetic</p>",
    "<p>The intersection of machine learning and natural language processing has yielded remarkable advancements in recent years, fundamentally transforming how we approach text classification, sentiment analysis, and information extraction tasks.</p>",
    "<p>my cat just knocked over my coffee onto my keyboard again. third time this week. i think she's doing it on purpose at this point</p>"
  ),
  stringsAsFactors = FALSE
)

hai_labels <- toJSON(human_ai$annotation_labels, auto_unbox = TRUE)
hai_layout <- toJSON(human_ai$button_layout, auto_unbox = TRUE)
hai_hash   <- digest("Classify if this response was written by a human or AI",
                     algo = "md5", serialize = FALSE)

for (i in seq_len(nrow(hai_items))) {
  dbExecute(con,
    "INSERT OR IGNORE INTO items
     (id, instruction_hash, annotator_id, annotation_instruction,
      annotation_html, annotation_labels, button_layout)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      hai_items$id[i], hai_hash, "wschulz",
      hai_items$instruction[i], hai_items$html[i],
      hai_labels, hai_layout
    )
  )
}

# --- Test items: sentiment format -----------------------------------------
sent_items <- data.frame(
  id = paste0("test_sent_", 1:5),
  instruction = rep("What is the sentiment of this text?", 5),
  html = c(
    "<p>This is the best pizza I've ever had in my life. Absolutely incredible crust, perfect sauce, and the mozzarella was heavenly.</p>",
    "<p>The meeting went about as expected. Nothing particularly exciting but nothing went wrong either.</p>",
    "<p>I am so frustrated with this software. It crashes every five minutes and the support team is completely unresponsive.</p>",
    "<p>Just got back from vacation and honestly it was fine. Some nice moments, some boring ones. Pretty standard trip.</p>",
    "<p>I can't believe how generous the community has been! We raised twice our fundraising goal in just one week!</p>"
  ),
  stringsAsFactors = FALSE
)

sent_labels <- toJSON(sentiment$annotation_labels, auto_unbox = TRUE)
sent_layout <- toJSON(sentiment$button_layout, auto_unbox = TRUE)
sent_hash   <- digest("What is the sentiment of this text?",
                      algo = "md5", serialize = FALSE)

for (i in seq_len(nrow(sent_items))) {
  dbExecute(con,
    "INSERT OR IGNORE INTO items
     (id, instruction_hash, annotator_id, annotation_instruction,
      annotation_html, annotation_labels, button_layout)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      sent_items$id[i], sent_hash, "wschulz",
      sent_items$instruction[i], sent_items$html[i],
      sent_labels, sent_layout
    )
  )
}

# --- Verify ---------------------------------------------------------------
n_items <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM items")$n
n_users <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
wal     <- dbGetQuery(con, "PRAGMA journal_mode;")[[1]]

cat(sprintf("\nDone! Seeded database:\n  items: %d\n  users: %d\n  journal_mode: %s\n", n_items, n_users, wal))
cat(sprintf("  db: %s\n", db_path))

dbDisconnect(con)
