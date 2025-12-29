# fe

`fe` is a commandline secrets manager that seals and unseals files using available hardware-backed enclaves (Secure Enclave on macOS first, TPM 2.0 via future backends).

## Usage
```
fe [-v] [--nocolor] --seal|--unseal <file>
```

- Use `--seal` to encrypt and seal a file to enclave hardware.
- Use `--unseal` to decrypt and restore a previously sealed file.
- Use `-` to read from stdin or write to stdout.

## Examples
```
fe --seal secrets.txt
fe --unseal secrets.txt.sealed
cat secrets.txt | fe --seal - > secrets.txt.sealed
fe --unseal - < secrets.txt.sealed > secrets.txt
fe -v --seal secrets.txt
fe -v --nocolor --unseal secrets.txt.sealed
```

## Logging and Debugging
- Logging is off by default; pass `-v` to enable.
- Colorized log output is enabled by default; pass `--nocolor` to disable.
- Errors should be actionable and include backend identifiers when relevant.

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

Planned: integration tests for stdin/stdout and a mock enclave backend for CI.

## Platform Notes
- macOS Secure Enclave is the initial supported backend.
- Additional backends (TPM 2.0) will be added via the enclave interface.

## Future Flags
- `--backend <id>` to select a specific backend (override auto-detect).
- `--format-version <n>` to force a specific envelope version for compatibility testing.
