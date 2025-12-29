# AI State (src)

Last updated: initial scaffolding created.

## Files added
- `src/cli.rs`: CLI argument struct and mode enum (no parsing yet).
- `src/logging.rs`: log level enum and options holder.
- `src/io.rs`: stdio sentinel constant and helper.
- `src/format.rs`: envelope header + payload struct, magic/version constants, serde model.
- `src/enclave/mod.rs`: enclave trait, sealed key model, and error enum.
- `src/enclave/secure_enclave.rs`: platform check helper.

## Pending work
- Implement CLI parsing and wire `main` to command dispatch.
- Implement envelope encode/decode, validation, and versioning.
- Add Secure Enclave key wrapping logic and a mock backend for tests.
 - Implement IO read/write helpers and atomic file writes.

## Recent changes
- Added CLI parsing with clap, plus CLI tests (`src/cli.rs`).
- Added logging formatter and tests (`src/logging.rs`).
- Added envelope encode/decode + validation and tests (`src/format.rs`).
- Added stdio helper test (`src/io.rs`).
- Wired `src/main.rs` to parse args and initialize logging.
