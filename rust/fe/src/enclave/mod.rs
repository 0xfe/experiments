pub mod secure_enclave;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SealedKey {
    pub backend_id: String,
    pub key_ref: Vec<u8>,
    pub wrapped_key: Vec<u8>,
    pub wrap_algo: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnclaveError {
    Unavailable,
    Unsupported,
    InvalidSealedKey,
    Backend(String),
}

pub trait Enclave {
    fn backend_id(&self) -> &'static str;
    fn seal_key(&self, data_key: &[u8]) -> Result<SealedKey, EnclaveError>;
    fn unseal_key(&self, sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError>;
}
