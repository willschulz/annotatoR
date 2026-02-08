#!/usr/bin/env bash
# -----------------------------------------------------------
# Configure Tailscale Serve for annotatoR
#
# This makes the app available at:
#   https://datascience.manx-celsius.ts.net/label/
#
# Tailscale Serve handles:
#   - HTTPS termination (auto Let's Encrypt via Tailscale)
#   - WebSocket proxying
#
# Prerequisites:
#   - tailscale is installed and logged in
#   - the machine has MagicDNS hostname "datascience"
#
# Run once; persists across reboots with --bg.
# -----------------------------------------------------------

set -euo pipefail

# Ensure the hostname is "datascience" (revert if changed)
echo "Setting Tailscale hostname to 'datascience'..."
sudo tailscale set --hostname=datascience

echo "Configuring Tailscale Serve to proxy /label/ -> http://127.0.0.1:3839 ..."
sudo tailscale serve --bg --set-path /label/ http://127.0.0.1:3839

echo ""
echo "Done. The annotator is now reachable at:"
echo "  https://datascience.manx-celsius.ts.net/label/"
echo ""
echo "To check status:  tailscale serve status"
echo "To stop serving:  tailscale serve --remove /label/"
