#!/usr/bin/env bash
set -euo pipefail

bin_path="${BIN_PATH:-target/debug/fe}"
entitlements="${ENTITLEMENTS:-entitlements.plist}"
sign_id="${SIGN_ID:-}"

if [[ -z "${sign_id}" ]]; then
  echo "SIGN_ID is required (e.g., \"Developer ID Application: Your Name (TEAMID)\")" >&2
  exit 1
fi

if [[ ! -f "${bin_path}" ]]; then
  echo "Binary not found: ${bin_path}" >&2
  exit 1
fi

if [[ ! -f "${entitlements}" ]]; then
  echo "Entitlements file not found: ${entitlements}" >&2
  exit 1
fi

codesign --force --sign "${sign_id}" --entitlements "${entitlements}" "${bin_path}"
codesign --verify --verbose=2 "${bin_path}"
echo "Signed ${bin_path}"
