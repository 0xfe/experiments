//! Entry point for the `fe` CLI, wiring argument parsing to enclave operations.

// CLI parsing and validated options.
mod cli;
// Enclave backend registry and implementations.
mod enclave;
// Envelope format for sealed data and associated validation helpers.
mod format;
// IO utilities for stdin/stdout and atomic file writes.
mod io;
// Logging helpers with optional verbosity and color.
mod logging;
// High-level seal/unseal operations shared by the CLI.
mod ops;

fn main() {
    // Parse CLI arguments; clap handles detailed help, we just exit with a code.
    let args = match cli::Args::try_parse() {
        Ok(args) => args,
        Err(err) => {
            let _ = err.print();
            std::process::exit(2);
        }
    };
    // Initialize the logger based on user preferences.
    let logger = logging::Logger::new(logging::LogOptions {
        verbose: args.verbose,
        color: args.color,
    });
    // Backward-compat support for the deprecated FE_BACKEND env var.
    let env_backend = std::env::var("FE_BACKEND").ok();
    if env_backend.is_some() && args.backend.is_none() {
        logger.warn("FE_BACKEND is deprecated; use --backend instead");
    }
    let backend_override = args.backend.as_deref().or(env_backend.as_deref());
    // Load all known enclave backends so we can pick an available one.
    let registry = enclave::Registry::default();
    // Execute the requested sealing/unsealing operation.
    let result = match args.mode {
        cli::Mode::Seal => ops::seal_path(&registry, &logger, &args.file, backend_override),
        cli::Mode::Unseal => ops::unseal_path(&registry, &logger, &args.file),
    };
    if let Err(err) = result {
        // Errors are printed as single-line log messages for scripting friendliness.
        let line = logger.format_line(logging::Level::Error, &err.to_string());
        eprintln!("{line}");
        match err {
            ops::OpsError::Enclave(enclave::EnclaveError::Unavailable)
            | ops::OpsError::Enclave(enclave::EnclaveError::BackendNotFound(_)) => {
                // On backend selection failures, help users discover options.
                let available = registry.available_backend_ids().join(", ");
                if !available.is_empty() {
                    eprintln!("available backends: {available}");
                }
            }
            _ => {}
        }
        std::process::exit(1);
    }
}
