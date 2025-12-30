//! Mock enclave backend used for tests and local development.

use super::{Enclave, EnclaveError, SealedKey};

/// Simple, deterministic mock enclave that reverses bytes for wrapping.
#[derive(Debug, Default, Clone, Copy)]
pub struct MockEnclave;

impl MockEnclave {
    /// Construct a mock enclave instance.
    pub fn new() -> Self {
        Self
    }
}

impl Enclave for MockEnclave {
    /// Stable backend identifier for test envelopes.
    fn backend_id(&self) -> &'static str {
        "mock"
    }

    /// "Seal" by reversing bytes so unsealing can reverse again.
    fn seal_key(&self, data_key: &[u8]) -> Result<SealedKey, EnclaveError> {
        let wrapped_key = data_key.iter().rev().copied().collect();
        Ok(SealedKey {
            backend_id: self.backend_id().to_string(),
            key_ref: b"mock-key".to_vec(),
            wrapped_key,
            wrap_algo: "reverse-bytes".to_string(),
        })
    }

    /// "Unseal" by reversing bytes back into the original key.
    fn unseal_key(&self, sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError> {
        if sealed.backend_id != self.backend_id() {
            return Err(EnclaveError::InvalidSealedKey);
        }
        Ok(sealed.wrapped_key.iter().rev().copied().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::MockEnclave;
    use crate::enclave::Enclave;

    #[test]
    fn seals_and_unseals_round_trip() {
        let enclave = MockEnclave::new();
        let data_key = b"data-key";
        let sealed = enclave.seal_key(data_key).expect("seal");
        let unsealed = enclave.unseal_key(&sealed).expect("unseal");
        assert_eq!(unsealed, data_key);
    }
}
