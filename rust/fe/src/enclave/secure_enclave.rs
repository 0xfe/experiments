#[derive(Debug, Default, Clone, Copy)]
pub struct SecureEnclave;

impl SecureEnclave {
    pub fn is_supported_platform() -> bool {
        cfg!(target_os = "macos")
    }
}
