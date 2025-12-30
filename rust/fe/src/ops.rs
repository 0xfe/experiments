//! Sealing and unsealing operations that combine enclave wrapping and AEAD.

use crate::enclave::{Enclave, EnclaveError, Registry, SealedKey};
use crate::format::{aad_bytes, decode_envelope, encode_envelope, new_header, Envelope};
use crate::io::{read_input, write_output, IoError};
use crate::logging::Logger;
use chacha20poly1305::aead::{Aead, KeyInit, Payload, rand_core::OsRng, rand_core::RngCore};
use chacha20poly1305::{ChaCha20Poly1305, Key, Nonce};

/// AEAD algorithm identifier embedded in the envelope header.
const AEAD_ALGO: &str = "chacha20poly1305";
/// ChaCha20-Poly1305 nonce length in bytes.
const NONCE_LEN: usize = 12;
/// ChaCha20-Poly1305 key length in bytes.
const KEY_LEN: usize = 32;

/// Errors produced by sealing/unsealing operations.
#[derive(Debug)]
pub enum OpsError {
    /// IO-related errors (stdin/stdout, filesystem).
    Io(IoError),
    /// Envelope encoding/decoding failures.
    Format(crate::format::FormatError),
    /// Enclave backend failures.
    Enclave(EnclaveError),
    /// Cryptographic errors surfaced by AEAD operations.
    Crypto(String),
    /// Nonce length mismatches in the envelope.
    InvalidNonceLength(usize),
    /// Unsupported AEAD algorithm requested in the envelope.
    UnsupportedAead(String),
}

impl std::fmt::Display for OpsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(err) => write!(f, "{err}"),
            Self::Format(err) => write!(f, "{err}"),
            Self::Enclave(err) => write!(f, "{err}"),
            Self::Crypto(message) => write!(f, "crypto error: {message}"),
            Self::InvalidNonceLength(len) => write!(f, "invalid nonce length: {len}"),
            Self::UnsupportedAead(algo) => write!(f, "unsupported aead: {algo}"),
        }
    }
}

impl std::error::Error for OpsError {}

impl From<IoError> for OpsError {
    fn from(err: IoError) -> Self {
        Self::Io(err)
    }
}

impl From<crate::format::FormatError> for OpsError {
    fn from(err: crate::format::FormatError) -> Self {
        Self::Format(err)
    }
}

impl From<EnclaveError> for OpsError {
    fn from(err: EnclaveError) -> Self {
        Self::Enclave(err)
    }
}

/// Seal bytes from a path using the selected backend, writing an envelope next to it.
pub fn seal_path(
    registry: &Registry,
    logger: &Logger,
    path: &str,
    backend_override: Option<&str>,
) -> Result<(), OpsError> {
    // Read input bytes from file or stdin.
    let input = read_input(path)?;
    // Select backend based on availability or explicit override.
    let backend = registry.select_for_seal(backend_override)?;
    logger.info(&format!(
        "sealing {} bytes with backend {}",
        input.len(),
        backend.backend_id()
    ));
    // Seal bytes and write the envelope back to the target.
    let sealed = seal_bytes(backend, &input)?;
    write_output(path, &sealed)?;
    Ok(())
}

/// Unseal bytes from a path and write plaintext back to the same location.
pub fn unseal_path(
    registry: &Registry,
    logger: &Logger,
    path: &str,
) -> Result<(), OpsError> {
    // Read envelope bytes from file or stdin.
    let input = read_input(path)?;
    // Decode the envelope to select the correct backend.
    let envelope = decode_envelope(&input)?;
    let backend = registry.by_id(&envelope.header.backend_id)?;
    logger.info(&format!(
        "unsealing {} bytes with backend {}",
        envelope.ciphertext.len(),
        backend.backend_id()
    ));
    // Unseal and write back plaintext.
    let plaintext = unseal_envelope(backend, &envelope)?;
    write_output(path, &plaintext)?;
    Ok(())
}

/// Seal an in-memory plaintext, returning CBOR envelope bytes.
pub fn seal_bytes(enclave: &dyn Enclave, plaintext: &[u8]) -> Result<Vec<u8>, OpsError> {
    let envelope = seal_envelope(enclave, plaintext)?;
    Ok(encode_envelope(&envelope)?)
}

/// Unseal CBOR envelope bytes into plaintext.
pub fn unseal_bytes(enclave: &dyn Enclave, envelope_bytes: &[u8]) -> Result<Vec<u8>, OpsError> {
    let envelope = decode_envelope(envelope_bytes)?;
    unseal_envelope(enclave, &envelope)
}

/// Seal plaintext into an envelope using a freshly generated data key.
fn seal_envelope(enclave: &dyn Enclave, plaintext: &[u8]) -> Result<Envelope, OpsError> {
    // Generate and seal a fresh data key for this payload.
    let data_key = generate_data_key();
    let sealed_key = enclave.seal_key(&data_key)?;
    // Generate a random nonce and prepare header metadata.
    let nonce = generate_nonce();
    let header = new_header(
        sealed_key.backend_id.clone(),
        sealed_key.wrap_algo.clone(),
        AEAD_ALGO,
        nonce.to_vec(),
        sealed_key.wrapped_key.clone(),
        sealed_key.key_ref.clone(),
        Some(plaintext.len() as u64),
    );
    // Compute AAD over the header and encrypt with ChaCha20-Poly1305.
    let aad = aad_bytes(&header)?;
    let cipher = ChaCha20Poly1305::new(Key::from_slice(&data_key));
    let ciphertext = cipher
        .encrypt(
            Nonce::from_slice(&nonce),
            Payload {
                msg: plaintext,
                aad: &aad,
            },
        )
        .map_err(|err| OpsError::Crypto(err.to_string()))?;
    Ok(Envelope { header, ciphertext })
}

/// Unseal an envelope by validating metadata, unwrapping the key, and decrypting.
fn unseal_envelope(enclave: &dyn Enclave, envelope: &Envelope) -> Result<Vec<u8>, OpsError> {
    // Prevent algorithm substitution attacks.
    if envelope.header.aead_algo != AEAD_ALGO {
        return Err(OpsError::UnsupportedAead(
            envelope.header.aead_algo.clone(),
        ));
    }
    // Enforce the expected nonce length for ChaCha20-Poly1305.
    if envelope.header.nonce.len() != NONCE_LEN {
        return Err(OpsError::InvalidNonceLength(envelope.header.nonce.len()));
    }
    // Reconstruct the sealed key for the backend.
    let sealed_key = SealedKey {
        backend_id: envelope.header.backend_id.clone(),
        key_ref: envelope.header.key_ref.clone(),
        wrapped_key: envelope.header.sealed_key.clone(),
        wrap_algo: envelope.header.wrap_algo.clone(),
    };
    // Unwrap the data key using the selected backend.
    let data_key = enclave.unseal_key(&sealed_key)?;
    // Ensure the unwrapped key length matches the AEAD requirements.
    if data_key.len() != KEY_LEN {
        return Err(OpsError::Crypto("invalid data key length".to_string()));
    }
    // Decrypt using the header-derived AAD.
    let cipher = ChaCha20Poly1305::new(Key::from_slice(&data_key));
    let aad = aad_bytes(&envelope.header)?;
    let nonce = Nonce::from_slice(&envelope.header.nonce);
    cipher
        .decrypt(
            nonce,
            Payload {
                msg: envelope.ciphertext.as_slice(),
                aad: &aad,
            },
        )
        .map_err(|err| OpsError::Crypto(err.to_string()))
}

/// Generate a fresh random nonce for ChaCha20-Poly1305.
fn generate_nonce() -> [u8; NONCE_LEN] {
    let mut nonce = [0u8; NONCE_LEN];
    OsRng.fill_bytes(&mut nonce);
    nonce
}

/// Generate a fresh random data key for ChaCha20-Poly1305.
fn generate_data_key() -> [u8; KEY_LEN] {
    let mut key = [0u8; KEY_LEN];
    OsRng.fill_bytes(&mut key);
    key
}

#[cfg(test)]
mod tests {
    use super::{seal_bytes, unseal_bytes};
    use crate::enclave::mock::MockEnclave;

    #[test]
    fn seal_and_unseal_round_trip() {
        let enclave = MockEnclave::new();
        let plaintext = b"secret";
        let sealed = seal_bytes(&enclave, plaintext).expect("seal");
        let unsealed = unseal_bytes(&enclave, &sealed).expect("unseal");
        assert_eq!(unsealed, plaintext);
    }
}
