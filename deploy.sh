#!/usr/bin/env bash
# deploy.sh — deploy annotatoR to hydria after git push
#
# Usage (from ~/Desktop/homelab/annotatoR/ or ~/Documents/GitHub/annotatoR/):
#   ./deploy.sh
#
# What this does:
#   1. Checks the NAS-side repo for unexpected local edits (dirty working tree
#      would cause git pull to fail silently).
#   2. Pulls the latest main from GitHub into ~/projects/tools/annotatoR/ on hydria.
#   3. Reinstalls the R package (~2-3 min).
#   4. Restarts annotator.service (no password required — see /etc/sudoers.d/annotator-restart).
#   5. Verifies the service is active.
#
# Assumes:
#   - You have already committed and pushed your changes: git push origin main
#   - SSH access to wschulz@100.65.14.50 (via Tailscale)
#
# DB SAFETY: This script never touches state/, exports/, or the SQLite file.
# Git only updates tracked files; state/ is .gitignore'd.

set -euo pipefail

HYDRIA="wschulz@100.65.14.50"
REPO_DIR="~/projects/tools/annotatoR"
INSTALLED_PKG="~/R/x86_64-pc-linux-gnu-library/4.5/annotatoR"

echo "=== [1/4] Checking NAS-side repo for unexpected local edits ==="
dirty=$(ssh "$HYDRIA" "bash -lc 'cd $REPO_DIR && git status --short'")
if [ -n "$dirty" ]; then
  echo "WARNING: NAS repo has local modifications:"
  echo "$dirty"
  echo "Inspect and git checkout -- <file> for any unintended changes before proceeding."
  read -r -p "Continue anyway? [y/N] " confirm
  [[ "${confirm,,}" == "y" ]] || { echo "Aborted."; exit 1; }
fi

echo "=== [2/4] Pulling latest main from GitHub ==="
ssh "$HYDRIA" "bash -lc 'cd $REPO_DIR && git pull origin main 2>&1'"

echo "=== [3/4] Reinstalling R package (devtools::install) ==="
ssh "$HYDRIA" "bash -lc 'Rscript -e \"devtools::install(\\\"$REPO_DIR/repo/\\\")\" 2>&1 | tail -5'"

echo "=== [4/4] Restarting annotator.service ==="
ssh "$HYDRIA" 'sudo systemctl restart annotator'

echo "=== Verifying ==="
sleep 2
status=$(ssh "$HYDRIA" 'systemctl is-active annotator')
if [ "$status" = "active" ]; then
  echo "annotator.service is active."
  ssh "$HYDRIA" "tail -5 ~/projects/tools/annotatoR/state/logs/annotator.log"
else
  echo "ERROR: annotator.service is NOT active after restart (status: $status)."
  ssh "$HYDRIA" "sudo journalctl -u annotator -n 20 --no-pager"
  exit 1
fi

echo ""
echo "Deploy complete. Service is running and listening on http://127.0.0.1:3839 (hydria)."
echo "Tailscale URL: https://datascience.manx-celsius.ts.net/label/"
