#!/usr/bin/env bash
# Codesign the CLI binary with entitlements required for Secure Enclave access.
set -euo pipefail

bin_path="${1:-}"
entitlements="${ENTITLEMENTS:-entitlements.plist}"
entitlements_to_use="${entitlements}"
sign_id="${SIGN_ID:-LOCALDEV001}"
app_id_prefix="${APP_ID_PREFIX:-LOCALDEV001.}"
tmp_entitlements=""

if [[ -z "${bin_path}" ]]; then
  echo "Usage: $0 /path/to/binary" >&2
  exit 1
fi

if ! security find-identity -p codesigning -v | rg -q "\"${sign_id}\""; then
  echo "Signing identity not found: ${sign_id}" >&2
  echo "Run ./scripts/new-codesign-cert.sh or set SIGN_ID." >&2
  exit 1
fi

cert_pem="$(security find-certificate -c "${sign_id}" -p 2>/dev/null || true)"
if [[ -n "${cert_pem}" ]]; then
  subject="$(printf '%s' "${cert_pem}" | openssl x509 -noout -subject 2>/dev/null || true)"
  team_id="$(printf '%s' "${subject}" | sed -n 's/.*OU=\\([^,/]*\\).*/\\1/p')"
  if [[ -z "${team_id}" ]]; then
    subject="$(printf '%s' "${cert_pem}" | openssl x509 -noout -subject -nameopt RFC2253 2>/dev/null || true)"
    team_id="$(printf '%s' "${subject}" | sed -n 's/.*OU=\\([^,]*\\).*/\\1/p')"
  fi
  if [[ -n "${team_id}" ]]; then
    app_id_prefix="${team_id}."
  fi
fi

if [[ ! -f "${bin_path}" ]]; then
  echo "Binary not found: ${bin_path}" >&2
  exit 1
fi

if [[ ! -f "${entitlements}" ]]; then
  script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  echo "Entitlements file not found: ${entitlements}; generating defaults." >&2
  if [[ -n "${app_id_prefix}" ]]; then
    ENTITLEMENTS_OUT="${entitlements}" APP_ID_PREFIX="${app_id_prefix}" \
      "${script_dir}/create-entitlements.sh" >/dev/null
  else
    ENTITLEMENTS_OUT="${entitlements}" "${script_dir}/create-entitlements.sh" >/dev/null
  fi
elif rg -q '\$\(\s*AppIdentifierPrefix\s*\)' "${entitlements}"; then
  tmp_entitlements="$(mktemp)"
  trap 'rm -f "$tmp_entitlements"' EXIT
  sed 's/$(AppIdentifierPrefix)/'"${app_id_prefix}"'/g' "${entitlements}" > "${tmp_entitlements}"
  entitlements_to_use="${tmp_entitlements}"
fi

codesign --force --sign "${sign_id}" --entitlements "${entitlements_to_use}" "${bin_path}"
codesign --verify --verbose=2 "${bin_path}"
echo "Signed ${bin_path}"
