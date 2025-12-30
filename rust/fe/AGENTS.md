# Repository Guidelines

# About `fe`

FE is a simple commandline secrets manager that seals secrets using available enclave hardware, such as a TPM 2.0 chip, or a Mac enclave.

## Agent guidelines

- This is meant to be robust secure code that is able to withstand the most complex attacks.
- Write clear readable code, and make sure it is well tested.
- Write simple clear interfaces, and try to minimize duplication as much as possible. (Create and use common helper libraries as needed.)

### Commenting

- Comment heavily, particularly all interfaces, constants, declarations, explaining what they do along with a clear justification.
- Comment complex functions, explaining each code path in detail, and justifying algorithmic choices.
- Add a top-level comment to every file with a summary of the contents of the file.

### State tracking

Maintain a `ai-state.md` file in each subdirectory, and keep it updated as changes are made. Put whatever information you need to be able to be able to quickly understand and update code with a fresh context. Feel free to include file names, line numbers, interfaces, etc.

## Project Structure & Module Organization

This is a small Rust binary crate. The entry point is `src/main.rs`, and `Cargo.toml` defines the package metadata and dependencies. Build artifacts are generated under `target/` and should not be edited. If the code grows, prefer adding modules under `src/` (e.g., `src/math.rs`) and keeping `main` as a thin orchestrator.

## Build, Test, and Development Commands

- `cargo build`: Compile the crate in debug mode.
- `cargo run`: Build and run the binary locally.
- `cargo build --release`: Compile optimized artifacts.
- `cargo test`: Run unit and integration tests (none are present yet).
- `cargo fmt`: Format code with rustfmt.
- `cargo clippy`: Lint for common Rust issues.

## Coding Style & Naming Conventions

Use Rust 2024 edition defaults and rely on `cargo fmt` for formatting. Follow standard Rust naming: `snake_case` for functions/variables/modules, `CamelCase` for types/traits, and `SCREAMING_SNAKE_CASE` for constants. Prefer small, focused functions and explicit error handling over `unwrap()` in production paths.

## Testing Guidelines

Use `#[test]` functions for unit tests alongside the code in `src/`, and add integration tests under `tests/` when behavior spans modules. Keep test names descriptive (e.g., `parses_float_inputs`) and cover boundary cases. Run `cargo test` before submitting changes.
