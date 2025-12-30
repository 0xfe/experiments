//! Minimal logger that formats single-line messages for stderr output.

/// Severity levels used by the logger.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    /// Error-level messages.
    Error,
    /// Warning-level messages.
    Warn,
    /// Informational messages.
    Info,
}

/// Logger configuration flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LogOptions {
    /// Print log messages (when false, suppress output).
    pub verbose: bool,
    /// Emit ANSI color codes in log labels.
    pub color: bool,
}

/// Logger instance that renders formatted log lines.
#[derive(Debug, Clone)]
pub struct Logger {
    options: LogOptions,
}

impl Logger {
    /// Construct a logger with the supplied options.
    pub fn new(options: LogOptions) -> Self {
        Self { options }
    }

    /// Emit a log line if verbosity is enabled.
    pub fn log(&self, level: Level, message: &str) {
        if !self.options.verbose {
            return;
        }
        eprintln!("{}", self.format_line(level, message));
    }

    /// Convenience method for informational logs.
    pub fn info(&self, message: &str) {
        self.log(Level::Info, message);
    }

    /// Convenience method for warning logs.
    pub fn warn(&self, message: &str) {
        self.log(Level::Warn, message);
    }

    /// Convenience method for error logs.
    pub fn error(&self, message: &str) {
        self.log(Level::Error, message);
    }

    /// Format a log line with optional ANSI color labels.
    pub fn format_line(&self, level: Level, message: &str) -> String {
        let label = match level {
            Level::Error => "error",
            Level::Warn => "warn",
            Level::Info => "info",
        };
        if self.options.color {
            let color = match level {
                Level::Error => "\x1b[31m",
                Level::Warn => "\x1b[33m",
                Level::Info => "\x1b[34m",
            };
            format!("{color}{label}\x1b[0m: {message}")
        } else {
            format!("{label}: {message}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Level, LogOptions, Logger};

    #[test]
    fn formats_with_color() {
        let logger = Logger::new(LogOptions {
            verbose: true,
            color: true,
        });
        let line = logger.format_line(Level::Info, "hello");
        assert!(line.contains("\x1b["));
        assert!(line.ends_with("hello"));
    }

    #[test]
    fn formats_without_color() {
        let logger = Logger::new(LogOptions {
            verbose: true,
            color: false,
        });
        let line = logger.format_line(Level::Warn, "msg");
        assert!(!line.contains("\x1b["));
        assert_eq!(line, "warn: msg");
    }
}
