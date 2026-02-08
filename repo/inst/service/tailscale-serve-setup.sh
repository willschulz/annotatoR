#!/usr/bin/env bash
# -----------------------------------------------------------
# Configure Tailscale Serve for annotatoR
#
# This makes the app available at:
#   https://label.manx-celsius.ts.net/
#
# Tailscale Serve handles:
#   - HTTPS termination (auto Let's Encrypt via Tailscale)
#   - WebSocket proxying
#
# Prerequisites:
#   - tailscale is installed and logged in
#   - the machine has MagicDNS hostname "label"
#     (set via: tailscale set --hostname=label)
#
# Run once; persists across reboots with --bg.
# -----------------------------------------------------------

set -euo pipefail

# Ensure the hostname is "label" so the FQDN becomes label.manx-celsius.ts.net
echo "Setting Tailscale hostname to 'label'..."
sudo tailscale set --hostname=label

echo "Configuring Tailscale Serve to proxy / -> http://127.0.0.1:3839 ..."
sudo tailscale serve --bg --set-path / http://127.0.0.1:3839

echo ""
echo "Done. The annotator is now reachable at:"
echo "  https://label.manx-celsius.ts.net/"
echo ""
echo "To check status:  tailscale serve status"
echo "To stop serving:  tailscale serve --remove /"
