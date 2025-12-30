//! IO helpers for reading stdin/stdout and atomically writing files.

use std::io::{Read, Write};

/// Sentinel path value indicating stdin/stdout should be used.
pub const STDIO_PATH: &str = "-";

/// Return true if the provided path denotes stdio.
pub fn is_stdio(path: &str) -> bool {
    path == STDIO_PATH
}

/// IO errors with context for temp file creation failures.
#[derive(Debug)]
pub enum IoError {
    /// Wrapper around underlying std::io::Error.
    Io(std::io::Error),
    /// Too many temp file name collisions while attempting an atomic write.
    TempFileAttemptsExceeded,
}

impl std::fmt::Display for IoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(err) => write!(f, "io error: {err}"),
            Self::TempFileAttemptsExceeded => write!(f, "failed to create temp file"),
        }
    }
}

impl std::error::Error for IoError {}

impl From<std::io::Error> for IoError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

/// Read bytes from a file path or stdin when the path is "-".
pub fn read_input(path: &str) -> Result<Vec<u8>, IoError> {
    if is_stdio(path) {
        let mut input = Vec::new();
        std::io::stdin().read_to_end(&mut input)?;
        return Ok(input);
    }
    Ok(std::fs::read(path)?)
}

/// Write bytes to a file path or stdout when the path is "-".
pub fn write_output(path: &str, bytes: &[u8]) -> Result<(), IoError> {
    if is_stdio(path) {
        let mut stdout = std::io::stdout();
        stdout.write_all(bytes)?;
        stdout.flush()?;
        return Ok(());
    }
    let path = std::path::Path::new(path);
    write_atomic(path, bytes)
}

/// Atomically write bytes to a file by writing and renaming a temp file.
fn write_atomic(path: &std::path::Path, bytes: &[u8]) -> Result<(), IoError> {
    // Use the target directory for temp files to ensure rename is atomic.
    let dir = path.parent().unwrap_or_else(|| std::path::Path::new("."));
    let mut attempt = 0u32;
    loop {
        // Include PID and attempt to reduce collisions in the same directory.
        let tmp_name = format!(".fe.tmp.{}.{}", std::process::id(), attempt);
        let tmp_path = dir.join(tmp_name);
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&tmp_path)
        {
            Ok(mut file) => {
                // Write, sync, and then rename into place.
                if let Err(err) = write_and_sync(&mut file, bytes) {
                    let _ = std::fs::remove_file(&tmp_path);
                    return Err(err);
                }
                std::fs::rename(&tmp_path, path)?;
                sync_dir(dir)?;
                return Ok(());
            }
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => {
                // Retry with a new temp name; cap attempts to avoid infinite loops.
                attempt += 1;
                if attempt > 100 {
                    return Err(IoError::TempFileAttemptsExceeded);
                }
            }
            Err(err) => return Err(IoError::Io(err)),
        }
    }
}

/// Ensure data is persisted to disk before renaming.
fn write_and_sync(file: &mut std::fs::File, bytes: &[u8]) -> Result<(), IoError> {
    use std::io::Write;
    file.write_all(bytes)?;
    file.sync_all()?;
    Ok(())
}

/// Sync the directory entry to ensure the rename is durable.
fn sync_dir(path: &std::path::Path) -> Result<(), IoError> {
    let dir = std::fs::File::open(path)?;
    dir.sync_all()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{is_stdio, read_input, write_output};
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn detects_stdio_path() {
        assert!(is_stdio("-"));
        assert!(!is_stdio("file.txt"));
    }

    #[test]
    fn writes_and_reads_file() {
        let dir = temp_dir();
        let path = dir.join("sample.bin");
        let payload = b"hello world";
        write_output(path.to_str().expect("path"), payload).expect("write");
        let read_back = read_input(path.to_str().expect("path")).expect("read");
        assert_eq!(read_back, payload);
        fs::remove_file(&path).expect("remove file");
        fs::remove_dir(&dir).expect("remove dir");
    }

    #[test]
    fn atomic_write_does_not_leave_temp_files() {
        let dir = temp_dir();
        let path = dir.join("out.bin");
        write_output(path.to_str().expect("path"), b"data").expect("write");
        let entries: Vec<_> = fs::read_dir(&dir).expect("read dir").collect();
        assert_eq!(entries.len(), 1);
        fs::remove_file(&path).expect("remove file");
        fs::remove_dir(&dir).expect("remove dir");
    }

    fn temp_dir() -> std::path::PathBuf {
        let mut dir = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        dir.push(format!("fe-test-{}-{nanos}", std::process::id()));
        fs::create_dir(&dir).expect("create temp dir");
        dir
    }
}
