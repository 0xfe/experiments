# AI State

Last updated: YubiKey backend compiled; tests run.

## Current goals
- Produce a phased plan for the `fe` CLI (see `plan.md`).
- Future implementation should prioritize secure, readable Rust with strong tests.

## Key requirements
- CLI: `fe [-v] [--nocolor] --seal|--unseal file`, `-` means stdin/stdout.
- Must use physical hardware sealing (macOS Secure Enclave initially).
- Clear, extensible enclave interface for future backends (e.g., TPM 2.0).
- Logging is off by default; `-v` enables; colored output unless `--nocolor`.
- README should include usage, logging/debug, and developer build/test sections.

## Notes
- AGENTS.md mandates robust secure code and maintaining `ai-state.md` per subdirectory.
- When implementation starts, add `src/ai-state.md` and keep it current.
- Plan expanded with concrete module and envelope format sketch in `plan.md`.
- README outline created in `README.md` (pre-implementation).
- Added envelope field details, backend selection rules, and error expectations in `plan.md`.
- Added more README examples and common error guidance in `README.md`.
- Added README sections for Security Model and Future Flags.
- Began implementation scaffolding (modules, envelope structs, enclave trait) and added `src/ai-state.md`.
- Implemented CLI parsing and logging scaffolding with tests; added envelope encode/decode validation and tests.
- Added clap dependency and ran `cargo test` (passed, with unused warnings in stub modules).
- Added IO helpers with atomic writes and tests, plus a mock enclave backend for test usage.
- Added ops for sealing/unsealing with ChaCha20-Poly1305 and integration tests using mock backend.
- Added enclave registry selection with backend override via `FE_BACKEND`.
- Ran `cargo test --features mock-enclave` (passes; warnings remain for unused stubs).
- Implemented Secure Enclave wrapping/unwrapping on macOS and added `--backend` CLI flag.
- Added ignored Secure Enclave integration test and README manual validation steps.
- Added deprecation warning for `FE_BACKEND` usage.
- Ran `cargo test --features mock-enclave` after backend changes (secure enclave test ignored).
- Added Secure Enclave missing-entitlement error mapping and README codesign guidance.
- Configured Secure Enclave key creation to use the Data Protection keychain and reran tests.
- Added scripts for entitlements and codesigning, plus a justfile for build/test/release workflows.
- Added YubiKey backend (PIV RSA wrapping) and ignored integration test; README includes setup guidance.
- Ran `cargo test --features mock-enclave` after adding YubiKey backend (YubiKey test ignored).
- Updated README entitlements guidance to recommend a real signing identity.
