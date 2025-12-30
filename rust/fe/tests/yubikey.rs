#[cfg(not(target_os = "windows"))]
mod tests {
    use std::fs;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    #[ignore]
    fn seal_and_unseal_with_yubikey() {
        let dir = temp_dir();
        let path = dir.join("data.bin");
        let input = b"yubikey test payload";
        fs::write(&path, input).expect("write");

        let status = fe_cmd()
            .arg("--backend")
            .arg("yubikey")
            .arg("--seal")
            .arg(path.to_str().expect("path"))
            .status()
            .expect("seal status");
        assert!(status.success());

        let status = fe_cmd()
            .arg("--backend")
            .arg("yubikey")
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

    fn fe_cmd() -> Command {
        let exe = env!("CARGO_BIN_EXE_fe");
        Command::new(exe)
    }

    fn temp_dir() -> std::path::PathBuf {
        let mut dir = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        dir.push(format!("fe-yk-test-{}-{nanos}", std::process::id()));
        fs::create_dir(&dir).expect("create dir");
        dir
    }
}
