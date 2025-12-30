#[cfg(feature = "mock-enclave")]
mod tests {
    use std::fs;
    use std::io::Write;
    use std::process::{Command, Stdio};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn seal_and_unseal_file() {
        let dir = temp_dir();
        let path = dir.join("data.bin");
        let input = b"hello from file";
        fs::write(&path, input).expect("write");

        let status = fe_cmd()
            .arg("--seal")
            .arg(path.to_str().expect("path"))
            .status()
            .expect("seal status");
        assert!(status.success());

        let sealed = fs::read(&path).expect("read sealed");
        assert_ne!(sealed, input);

        let status = fe_cmd()
            .arg("--unseal")
            .arg(path.to_str().expect("path"))
            .status()
            .expect("unseal status");
        assert!(status.success());

        let output = fs::read(&path).expect("read output");
        assert_eq!(output, input);

        fs::remove_file(&path).expect("remove file");
        fs::remove_dir(&dir).expect("remove dir");
    }

    #[test]
    fn seal_and_unseal_stdio() {
        let input = b"hello from stdin";

        let mut seal = fe_cmd()
            .arg("--seal")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn seal");
        seal.stdin
            .as_mut()
            .expect("stdin")
            .write_all(input)
            .expect("write stdin");
        let sealed = seal.wait_with_output().expect("seal output");
        assert!(sealed.status.success());

        let mut unseal = fe_cmd()
            .arg("--unseal")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn unseal");
        unseal
            .stdin
            .as_mut()
            .expect("stdin")
            .write_all(&sealed.stdout)
            .expect("write stdin");
        let output = unseal.wait_with_output().expect("unseal output");
        assert!(output.status.success());
        assert_eq!(output.stdout, input);
    }

    fn fe_cmd() -> Command {
        let exe = env!("CARGO_BIN_EXE_fe");
        let mut cmd = Command::new(exe);
        cmd.arg("--backend").arg("mock");
        cmd
    }

    fn temp_dir() -> std::path::PathBuf {
        let mut dir = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        dir.push(format!("fe-cli-test-{}-{nanos}", std::process::id()));
        fs::create_dir(&dir).expect("create dir");
        dir
    }
}
