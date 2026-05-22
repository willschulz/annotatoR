#!/usr/bin/env bash
# -----------------------------------------------------------
# Configure Tailscale Serve for annotatoR
#
# This makes the app available at:
#   https://datascience.manx-celsius.ts.net:8443/
#
# Served on a dedicated HTTPS port (8443) so that RStudio at port 443
# and annotatoR at port 8443 are distinct origins — required for Safari
# to create separate web apps for each service.
#
# Tailscale Serve handles:
#   - HTTPS termination (auto Let's Encrypt via Tailscale)
#   - WebSocket proxying
#
# Prerequisites:
#   - tailscale is installed and logged in
#   - the machine has MagicDNS hostname "datascience"
#   - Tailscale ACL allows tag:datascience:8443 for autogroup:member
#
# Run once; persists across reboots with --bg.
# -----------------------------------------------------------

set -euo pipefail

# Ensure the hostname is "datascience" (revert if changed)
echo "Setting Tailscale hostname to 'datascience'..."
sudo tailscale set --hostname=datascience

echo "Configuring Tailscale Serve to proxy port 8443 -> http://127.0.0.1:3839 ..."
sudo tailscale serve --bg --https=8443 http://127.0.0.1:3839

echo ""
echo "Done. The annotator is now reachable at:"
echo "  https://datascience.manx-celsius.ts.net:8443/"
echo ""
echo "To check status:  tailscale serve status"
echo "To stop serving:  tailscale serve --https=8443 off"
