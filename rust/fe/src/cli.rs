//! CLI argument parsing for the `fe` binary, with minimal validation and tests.

use clap::{ArgGroup, Parser};

/// Operation mode the CLI exposes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Seal plaintext into a sealed envelope.
    Seal,
    /// Unseal a sealed envelope back into plaintext.
    Unseal,
}

/// Parsed, validated CLI arguments used by the rest of the application.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Args {
    /// Which operation to perform (seal or unseal).
    pub mode: Mode,
    /// File path or "-" for stdin/stdout.
    pub file: String,
    /// Enable verbose logging to stderr.
    pub verbose: bool,
    /// Enable ANSI color in log output.
    pub color: bool,
    /// Optional backend override by ID.
    pub backend: Option<String>,
}

// Raw clap-driven arguments with flags and positional values.
#[derive(Debug, Parser)]
#[command(name = "fe", disable_help_subcommand = true)]
#[command(group(
    ArgGroup::new("mode")
        .required(true)
        .args(["seal", "unseal"])
        .multiple(false)
))]
struct RawArgs {
    /// Use sealing mode.
    #[arg(long)]
    seal: bool,
    /// Use unsealing mode.
    #[arg(long)]
    unseal: bool,
    /// Enable verbose logging.
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,
    /// Disable ANSI color in log output.
    #[arg(long = "nocolor")]
    nocolor: bool,
    /// Explicit backend override by identifier.
    #[arg(long = "backend")]
    backend: Option<String>,
    /// Path to input file or "-" for stdio.
    file: String,
}

impl Args {
    /// Parse arguments from process args, returning clap's rich error on failure.
    pub fn try_parse() -> Result<Self, clap::Error> {
        RawArgs::try_parse().map(Self::from)
    }

    /// Parse arguments from a provided iterator (used for tests).
    pub fn parse_from<I, T>(iter: I) -> Result<Self, clap::Error>
    where
        I: IntoIterator<Item = T>,
        T: Into<std::ffi::OsString> + Clone,
    {
        RawArgs::try_parse_from(iter).map(Self::from)
    }
}

impl From<RawArgs> for Args {
    fn from(raw: RawArgs) -> Self {
        // ArgGroup enforces exactly one mode flag.
        let mode = if raw.seal { Mode::Seal } else { Mode::Unseal };
        Self {
            mode,
            file: raw.file,
            verbose: raw.verbose,
            color: !raw.nocolor,
            backend: raw.backend,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Args, Mode};

    #[test]
    fn parses_seal_mode() {
        let args = Args::parse_from(["fe", "--seal", "input.txt"]).expect("parse");
        assert_eq!(args.mode, Mode::Seal);
        assert_eq!(args.file, "input.txt");
        assert!(!args.verbose);
        assert!(args.color);
    }

    #[test]
    fn parses_unseal_mode_with_stdio_and_verbose() {
        let args = Args::parse_from(["fe", "-v", "--unseal", "-"]).expect("parse");
        assert_eq!(args.mode, Mode::Unseal);
        assert_eq!(args.file, "-");
        assert!(args.verbose);
        assert!(args.color);
    }

    #[test]
    fn disables_color_when_requested() {
        let args = Args::parse_from(["fe", "--seal", "--nocolor", "in.bin"]).expect("parse");
        assert!(!args.color);
    }

    #[test]
    fn parses_backend_override() {
        let args = Args::parse_from(["fe", "--seal", "--backend", "mock", "in.bin"]).expect("parse");
        assert_eq!(args.backend.as_deref(), Some("mock"));
    }
}
