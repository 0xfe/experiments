#!/usr/bin/env bash
# Create a self-signed code signing certificate and import it into the login keychain.
set -euo pipefail

CERT_NAME="${1:-LOCALDEV001}"
KEYCHAIN="${HOME}/Library/Keychains/login.keychain-db"
TEAM_ID="${TEAM_ID:-LOCALDEV001}"
WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

KEY_PEM="$WORKDIR/codesign.key.pem"
CERT_PEM="$WORKDIR/codesign.cert.pem"
CERT_DER="$WORKDIR/codesign.cert.der"
OPENSSL_CONF="$WORKDIR/openssl.cnf"

echo "==> Creating local code-signing cert: $CERT_NAME"
echo "==> Team ID: $TEAM_ID"
echo "==> Working dir: $WORKDIR"

# Generate private key
openssl genrsa -out "$KEY_PEM" 2048 >/dev/null 2>&1

# OpenSSL config to ensure the cert is marked for code signing usage.
cat >"$OPENSSL_CONF" <<EOF
[req]
distinguished_name = dn
x509_extensions = v3_req
prompt = no

[dn]
CN = ${CERT_NAME}
OU = ${TEAM_ID}

[v3_req]
keyUsage = critical, digitalSignature
extendedKeyUsage = codeSigning
basicConstraints = CA:FALSE
EOF

# Self-signed cert (10 years) with code-signing EKU.
openssl req -new -x509 \
  -key "$KEY_PEM" \
  -out "$CERT_PEM" \
  -days 3650 \
  -subj "/CN=${CERT_NAME}/OU=${TEAM_ID}" \
  -config "$OPENSSL_CONF" >/dev/null 2>&1

# Convert cert to DER for 'security import'
openssl x509 -in "$CERT_PEM" -out "$CERT_DER" -outform DER >/dev/null 2>&1

echo "==> Importing into login keychain"
security import "$CERT_DER" -k "$KEYCHAIN" -A >/dev/null

# Trust the self-signed certificate for code signing.
security add-trusted-cert -p codeSign -d -r trustRoot -k "$KEYCHAIN" "$CERT_PEM" >/dev/null

# Import private key; allow codesign to use it when the binary exists on disk.
TRUSTED_APPS=()
for candidate in /usr/bin/codesign /usr/local/bin/codesign /opt/homebrew/bin/codesign; do
  if [[ -x "$candidate" ]]; then
    TRUSTED_APPS+=("-T" "$candidate")
  fi
done
if [[ ${#TRUSTED_APPS[@]} -eq 0 ]]; then
  echo "==> Warning: no codesign binary found to whitelist"
fi
security import "$KEY_PEM" \
  -k "$KEYCHAIN" \
  -P "" \
  "${TRUSTED_APPS[@]}" >/dev/null

echo "==> Available code-signing identities:"
security find-identity -p codesigning -v | sed 's/^/    /'

echo "==> Done. Sign a binary like:"
echo "    codesign --force --sign \"${CERT_NAME}\" /path/to/binary"
echo "==> For Secure Enclave access, sign with entitlements:"
echo "    BIN_PATH=/path/to/binary SIGN_ID=\"${CERT_NAME}\" ./scripts/codesign.sh"
