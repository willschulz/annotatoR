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
#   4. Publishes the collaborator-readable R library and export CLI.
#   5. Restarts annotator.service (no password required — see /etc/sudoers.d/annotator-restart).
#   6. Verifies the service is active.
#
# Assumes:
#   - You have already committed and pushed your changes: git push origin main
#   - SSH access to wschulz@100.65.14.50 (via Tailscale)
#
# DB SAFETY: This script never touches state/, exports/, or the SQLite file.
# Git only updates tracked files; state/ is .gitignore'd.

set -euo pipefail

HYDRIA="wschulz@100.65.14.50"
TRUENAS="root@100.108.222.2"
REPO_DIR="~/projects/tools/annotatoR"
INSTALLED_PKG="~/R/x86_64-pc-linux-gnu-library/4.5/annotatoR"
SHARED_R_LIB="/srv/projects/tools/annotatoR/r-library/4.5"
SHARED_R_LIB_TRUENAS="/mnt/tank/projects/active/tools/annotatoR/r-library/4.5"
SHARED_CLI_DIR="/srv/projects/tools/annotatoR/bin"
SHARED_CLI_DIR_TRUENAS="/mnt/tank/projects/active/tools/annotatoR/bin"

shared_artifacts_unlocked=0
lock_shared_artifacts() {
  local flag="$1"
  ssh "$TRUENAS" "
    for path in '$SHARED_R_LIB_TRUENAS' '$SHARED_CLI_DIR_TRUENAS'; do
      if [ -d \"\$path\" ]; then chattr -R '$flag' \"\$path\"; fi
    done
  "
}
relock_shared_artifacts() {
  if [ "$shared_artifacts_unlocked" -eq 1 ]; then
    echo "=== Relocking collaborator artifacts after interrupted deploy ==="
    lock_shared_artifacts "+i" || true
  fi
}
trap relock_shared_artifacts EXIT

echo "=== [1/5] Checking NAS-side repo for unexpected local edits ==="
dirty=$(ssh "$HYDRIA" "bash -lc 'cd $REPO_DIR && git status --short'")
if [ -n "$dirty" ]; then
  echo "WARNING: NAS repo has local modifications:"
  echo "$dirty"
  echo "Inspect and git checkout -- <file> for any unintended changes before proceeding."
  read -r -p "Continue anyway? [y/N] " confirm
  [[ "${confirm,,}" == "y" ]] || { echo "Aborted."; exit 1; }
fi

echo "=== [2/5] Pulling latest main from GitHub ==="
ssh "$HYDRIA" "bash -lc 'cd $REPO_DIR && git pull origin main 2>&1'"

echo "=== [3/5] Reinstalling R package (devtools::install) ==="
ssh "$HYDRIA" "bash -lc 'Rscript -e \"devtools::install(\\\"$REPO_DIR/repo/\\\")\" 2>&1 | tail -5'"

echo "=== [4/5] Publishing collaborator R library and CLI ==="
lock_shared_artifacts "-i"
shared_artifacts_unlocked=1
ssh "$HYDRIA" "bash -lc 'Rscript $REPO_DIR/repo/inst/service/install_shared_library.R $SHARED_R_LIB'"
ssh "$HYDRIA" "bash -lc '
  set -e
  install -d -m 0755 $SHARED_CLI_DIR
  cli_tmp=$SHARED_CLI_DIR/.annotator-export.\$\$
  install -m 0755 $REPO_DIR/repo/inst/cli/annotator_export.py \$cli_tmp
  mv -f \$cli_tmp $SHARED_CLI_DIR/annotator-export
'"
lock_shared_artifacts "+i"
shared_artifacts_unlocked=0

echo "=== [5/5] Restarting annotator.service ==="
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
echo "Tailscale URL: https://datascience.manx-celsius.ts.net:8443/"
