mod cli;
mod enclave;
mod format;
mod io;
mod logging;

fn main() {
    let args = match cli::Args::try_parse() {
        Ok(args) => args,
        Err(err) => {
            let _ = err.print();
            std::process::exit(2);
        }
    };
    let logger = logging::Logger::new(logging::LogOptions {
        verbose: args.verbose,
        color: args.color,
    });
    logger.info("fe initialized (operations not yet implemented)");
}
