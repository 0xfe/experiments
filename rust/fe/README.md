# fe

`fe` is a commandline secrets manager that seals and unseals files using available hardware-backed enclaves (Secure Enclave on macOS first, TPM 2.0 via future backends).

## Usage
```
fe [-v] [--nocolor] [--backend <id>] --seal|--unseal <file>
```

- Use `--seal` to encrypt and seal a file to enclave hardware.
- Use `--unseal` to decrypt and restore a previously sealed file.
- Use `-` to read from stdin or write to stdout.
- Use `--backend <id>` to force a specific backend (e.g., `secure_enclave`, `mock`).

## Examples
```
fe --seal secrets.txt
fe --unseal secrets.txt.sealed
cat secrets.txt | fe --seal - > secrets.txt.sealed
fe --unseal - < secrets.txt.sealed > secrets.txt
fe -v --seal secrets.txt
fe -v --nocolor --unseal secrets.txt.sealed
fe --backend mock --seal secrets.txt
```

## Logging and Debugging
- Logging is off by default; pass `-v` to enable.
- Colorized log output is enabled by default; pass `--nocolor` to disable.
- Errors should be actionable and include backend identifiers when relevant.
- `FE_BACKEND=<id>` can override backend selection when `--backend` is not provided (deprecated).

## Security Model
- `fe` protects secrets at rest by encrypting with a random data key and sealing that key to enclave hardware.
- The sealed output is only recoverable on hardware that can access the sealing key reference.
- Threats not covered: a compromised OS or a user session with access to the keychain may still decrypt data.
- Treat sealed files as sensitive; they contain metadata about the backend and key references.

## Common Errors
- `backend unavailable`: The required hardware backend is not detected on this machine.
- `key ref not found`: The key used to seal data is missing from the keychain.
- `malformed envelope`: The input file is not a valid `fe` sealed file or is corrupted.
Verify your input file, then re-run with `-v` for more context.

## Developer Guide
```
cargo build
cargo run -- --seal path/to/file
cargo test
cargo fmt
cargo clippy
```

Using just (optional):
```
just build
just test
just test-mock
just entitlements
just sign "Developer ID Application: Your Name (TEAMID)"
just release "Developer ID Application: Your Name (TEAMID)"
```

For tests that exercise sealing/unsealing flows, enable the mock enclave backend:
```
cargo test --features mock-enclave
```

Manual Secure Enclave check (macOS only, may prompt for Keychain access):
```
cargo run -- --backend secure_enclave --seal - < README.md > /tmp/fe.sealed
cargo run -- --backend secure_enclave --unseal - < /tmp/fe.sealed > /tmp/fe.unsealed
```

## Platform Notes
- macOS Secure Enclave is the initial supported backend.
- Secure Enclave keys are stored in the Keychain and may require access prompts.
- Secure Enclave key creation can require Keychain entitlements; OSStatus -34018 indicates missing entitlements.
- Secure Enclave keys are stored in the Data Protection keychain on macOS 10.15+.
- Additional backends (TPM 2.0) will be added via the enclave interface.

## YubiKey Backend (PIV)
- Backend id: `yubikey`
- Default slot: `9d` (PIV Key Management)
- Unseal requires a PIN: set `FE_YUBIKEY_PIN`.
- Optional: set `FE_YUBIKEY_SLOT` to override the slot (e.g., `9c`).

Example setup (using `ykman`):
```
ykman piv keys generate --algorithm RSA2048 9d pubkey.pem
ykman piv certificates generate 9d pubkey.pem
```

Then seal/unseal:
```
FE_YUBIKEY_PIN=123456 fe --backend yubikey --seal secrets.txt
FE_YUBIKEY_PIN=123456 fe --backend yubikey --unseal secrets.txt
```

### Secure Enclave Entitlements (macOS)
If you see `OSStatus -34018`, you likely need to codesign with Keychain entitlements. Example:
```
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>keychain-access-groups</key>
  <array>
    <string>$(AppIdentifierPrefix)com.example.fe</string>
  </array>
</dict>
</plist>
```
Then sign the binary with a real signing identity (ad-hoc signing may not grant entitlements):
```
codesign --force --sign "Developer ID Application: Your Name (TEAMID)" --entitlements entitlements.plist target/debug/fe
```

## Future Flags
- `--backend <id>` to select a specific backend (override auto-detect).
- `--format-version <n>` to force a specific envelope version for compatibility testing.
