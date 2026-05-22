# annotatoR Runbook

Operational reference for the annotatoR Shiny annotation service running on the datascience VM, served via Tailscale at `https://datascience.manx-celsius.ts.net:8443/`.

---

## Paths

| What | Path |
|---|---|
| R package source | `~/projects/tools/annotatoR/repo/` |
| SQLite database | `~/projects/tools/annotatoR/state/annotatoR.sqlite` |
| Cookie secret | `~/projects/tools/annotatoR/state/.cookie_secret` |
| Service logs | `~/projects/tools/annotatoR/state/logs/annotator.log` |
| Exports | `~/projects/tools/annotatoR/exports/` |
| Label formats | `repo/inst/label_formats/*.json` |
| systemd unit | `/etc/systemd/system/annotator.service` |

---

## First-time setup

### 1. Install the R package

```bash
cd ~/projects/tools/annotatoR/repo
Rscript -e "install.packages(c('shiny','shinyjs','shinyWidgets','fontawesome','DBI','RSQLite','pool','jsonlite','dplyr','dbplyr','digest','htmltools'), repos='https://cloud.r-project.org')"
Rscript -e "devtools::install('.')"
```

### 2. Seed the database (or skip if you already have data)

```bash
cd ~/projects/tools/annotatoR/repo
Rscript inst/seed_test_data.R
```

This creates the SQLite database at `state/annotatoR.sqlite`, bootstraps the schema, and inserts a small test batch assigned to `wschulz`.

### 3. Install the systemd service

```bash
sudo cp ~/projects/tools/annotatoR/repo/inst/service/annotator.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable annotator
sudo systemctl start annotator
```

### 4. Configure Tailscale Serve

```bash
bash ~/projects/tools/annotatoR/repo/inst/service/tailscale-serve-setup.sh
```

---

## Start / Stop / Restart

```bash
sudo systemctl start annotator
sudo systemctl stop annotator
sudo systemctl restart annotator
```

Check status:

```bash
sudo systemctl status annotator
journalctl -u annotator --since "1 hour ago"
tail -100 ~/projects/tools/annotatoR/state/logs/annotator.log
```

---

## Updating code

After editing files in `repo/`:

```bash
cd ~/projects/tools/annotatoR/repo
Rscript -e "devtools::install('.')"
sudo systemctl restart annotator
```

---

## Creating annotation batches from R

From any R session (e.g. inside a research project):

```r
library(annotatoR)

# Prepare a data frame with your items
df <- data.frame(
  id = paste0("item_", 1:100),
  annotation_instruction = "Classify if this text is human or AI",
  annotation_html = my_texts,  # your text column
  stringsAsFactors = FALSE
)

# Insert for two annotators, using the human_ai label format
annotator_create_batch(
  df,
  format     = "human_ai",
  annotators = c("alice@example.com", "bob@example.com"),
  db_path    = "~/projects/tools/annotatoR/state/annotatoR.sqlite"
)

# Make sure the annotators are registered
annotator_add_user("alice@example.com", display_name = "Alice",
                   db_path = "~/projects/tools/annotatoR/state/annotatoR.sqlite")
```

---

## Exporting labels

```r
library(annotatoR)
db <- "~/projects/tools/annotatoR/state/annotatoR.sqlite"

# As a data frame
labels <- annotator_export(db_path = db, completed_only = TRUE)

# Or write a timestamped CSV to exports/
annotator_export_csv(db_path = db, completed_only = TRUE)
```

---

## Backup

The entire state is a single SQLite file. SQLite in WAL mode is safe to copy while the app is running (it uses shared locks for reads):

```bash
cp ~/projects/tools/annotatoR/state/annotatoR.sqlite \
   ~/projects/tools/annotatoR/state/annotatoR.sqlite.bak.$(date +%Y%m%d)
```

Or use the SQLite backup API for a fully consistent snapshot:

```bash
sqlite3 ~/projects/tools/annotatoR/state/annotatoR.sqlite \
  ".backup ~/projects/tools/annotatoR/state/backup_$(date +%Y%m%d).sqlite"
```

---

## Restore

```bash
sudo systemctl stop annotator
cp /path/to/backup.sqlite ~/projects/tools/annotatoR/state/annotatoR.sqlite
sudo systemctl start annotator
```

---

## Tailscale Serve

Check proxy status:

```bash
tailscale serve status
```

Stop serving:

```bash
sudo tailscale serve --remove /
```

Re-enable:

```bash
sudo tailscale serve --bg --set-path / http://127.0.0.1:3839
```

---

## Troubleshooting

**App won't start**: Check logs at `state/logs/annotator.log`. Common causes:
- Missing R packages (re-run the install step)
- Port 3839 already in use (`lsof -i :3839`)
- SQLite file permissions

**Cookie login not working**: Delete `state/.cookie_secret` and restart. A new secret will be auto-generated (all existing cookies will be invalidated).

**Tailscale Serve not proxying**: Verify with `curl -v http://127.0.0.1:3839` that Shiny is actually listening. Then check `tailscale serve status`.

**Database locked errors**: The app uses WAL mode with a 5-second busy timeout. If you see lock errors, check for long-running queries (e.g. a migration script running concurrently).
