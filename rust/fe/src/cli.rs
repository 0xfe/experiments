use clap::{ArgGroup, Parser};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Seal,
    Unseal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Args {
    pub mode: Mode,
    pub file: String,
    pub verbose: bool,
    pub color: bool,
}

#[derive(Debug, Parser)]
#[command(name = "fe", disable_help_subcommand = true)]
#[command(group(
    ArgGroup::new("mode")
        .required(true)
        .args(["seal", "unseal"])
        .multiple(false)
))]
struct RawArgs {
    #[arg(long)]
    seal: bool,
    #[arg(long)]
    unseal: bool,
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,
    #[arg(long = "nocolor")]
    nocolor: bool,
    file: String,
}

impl Args {
    pub fn try_parse() -> Result<Self, clap::Error> {
        RawArgs::try_parse().map(Self::from)
    }

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
        let mode = if raw.seal { Mode::Seal } else { Mode::Unseal };
        Self {
            mode,
            file: raw.file,
            verbose: raw.verbose,
            color: !raw.nocolor,
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
}
