# AI State (src)

Last updated: Added heavy module/interface comments across src and captured enclave state.

## Files added
- `src/cli.rs`: CLI argument struct and mode enum (no parsing yet).
- `src/logging.rs`: log level enum and options holder.
- `src/io.rs`: stdio sentinel constant and helper.
- `src/format.rs`: envelope header + payload struct, magic/version constants, serde model.
- `src/enclave/mod.rs`: enclave trait, sealed key model, and error enum.
- `src/enclave/secure_enclave.rs`: Secure Enclave backend implementation.
- `src/ops.rs`: seal/unseal operations and encryption flow.

## Pending work
- Validate Secure Enclave flow on real hardware and document keychain prompts.
- Decide whether to keep `FE_BACKEND` env override or remove it after migration.

## Recent changes
- Added module-level and interface comments to all Rust files in `src/` for maintainability.
- Added `src/enclave/ai-state.md` to track enclave module details.
- Added CLI parsing with clap, plus CLI tests (`src/cli.rs`).
- Added logging formatter and tests (`src/logging.rs`).
- Added envelope encode/decode + validation and tests (`src/format.rs`).
- Added stdio helper test (`src/io.rs`).
- Wired `src/main.rs` to parse args and initialize logging.
- Added IO read/write helpers with atomic file writes and tests (`src/io.rs`).
- Added mock enclave backend with round-trip test (`src/enclave/mock.rs`).
- Added sealing/unsealing ops with ChaCha20-Poly1305 and envelope AAD (`src/ops.rs`).
- Added integration tests for file and stdio flows (`tests/cli.rs`).
- Added registry selection for backends and error reporting (`src/enclave/mod.rs`).
- Implemented Secure Enclave wrapping/unwrapping using `security-framework` (`src/enclave/secure_enclave.rs`).
- Added `--backend` CLI override and updated CLI tests (`src/cli.rs`).
- Added ignored Secure Enclave integration test (`tests/secure_enclave.rs`).
- Added deprecation warning for `FE_BACKEND` when used without `--backend`.
- Ran `cargo test --features mock-enclave` (secure enclave test ignored).
- Added Secure Enclave error mapping for missing entitlements and documented codesign steps in README.
- Configured Secure Enclave key creation to use Data Protection keychain and reran tests.
- Updated README entitlements guidance to recommend a real signing identity.
- Added YubiKey backend using PIV RSA wrapping (`src/enclave/yubikey.rs`).
- Added ignored YubiKey integration test (`tests/yubikey.rs`).
- Ran `cargo test --features mock-enclave` after adding YubiKey backend (YubiKey test ignored).
