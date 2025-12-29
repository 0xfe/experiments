#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LogOptions {
    pub verbose: bool,
    pub color: bool,
}

#[derive(Debug, Clone)]
pub struct Logger {
    options: LogOptions,
}

impl Logger {
    pub fn new(options: LogOptions) -> Self {
        Self { options }
    }

    pub fn log(&self, level: Level, message: &str) {
        if !self.options.verbose {
            return;
        }
        eprintln!("{}", self.format_line(level, message));
    }

    pub fn info(&self, message: &str) {
        self.log(Level::Info, message);
    }

    pub fn warn(&self, message: &str) {
        self.log(Level::Warn, message);
    }

    pub fn error(&self, message: &str) {
        self.log(Level::Error, message);
    }

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
