# AI State (scripts)

Last updated: Codesign defaults aligned to LOCALDEV001 and certificate OU parsing for entitlements.

## Files
- `scripts/new-codesign-cert.sh`: Creates a self-signed code-signing cert, imports it into the login keychain, trusts it for code signing, whitelists only existing codesign paths, marks the cert for code signing, and prints entitlements-aware signing guidance (defaults to LOCALDEV001).
- `scripts/codesign.sh`: Signs binaries with entitlements; defaults to LOCALDEV001, derives app ID prefix from cert OU, replaces AppIdentifierPrefix placeholders, and creates default entitlements if missing.
- `scripts/release.sh`: Release helper script.
- `scripts/create-entitlements.sh`: Generates entitlements for Secure Enclave use.
