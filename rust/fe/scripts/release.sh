#!/usr/bin/env bash
set -euo pipefail

entitlements="${ENTITLEMENTS:-entitlements.plist}"
sign_id="${SIGN_ID:-}"
bin_path="${BIN_PATH:-target/release/fe}"

if [[ -z "${sign_id}" ]]; then
  echo "SIGN_ID is required (e.g., \"Developer ID Application: Your Name (TEAMID)\")" >&2
  exit 1
fi

cargo build --release
BIN_PATH="${bin_path}" ENTITLEMENTS="${entitlements}" SIGN_ID="${sign_id}" \
  "$(dirname "$0")/codesign.sh"
echo "Release binary ready at ${bin_path}"
