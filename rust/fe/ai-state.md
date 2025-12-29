# AI State

Last updated: phase 1 expanded with CLI/logging/format helpers and tests; tests run.

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
