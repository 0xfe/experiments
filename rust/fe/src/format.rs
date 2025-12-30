//! Envelope encoding/decoding and format validation for sealed payloads.

use serde::{Deserialize, Serialize};

/// Magic bytes to identify a `fe` envelope payload.
pub const MAGIC: &[u8; 7] = b"FESEAL1";
/// Current envelope format version.
pub const VERSION: u8 = 1;

/// Full envelope header persisted alongside ciphertext.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvelopeHeader {
    /// Magic bytes to quickly identify the envelope format.
    #[serde(with = "serde_bytes")]
    pub magic: Vec<u8>,
    /// Envelope format version.
    pub version: u8,
    /// Backend identifier for the enclave used to wrap the data key.
    pub backend_id: String,
    /// Key-wrapping algorithm identifier for the backend.
    pub wrap_algo: String,
    /// AEAD algorithm used to encrypt the payload.
    pub aead_algo: String,
    /// AEAD nonce bytes used for encryption.
    #[serde(with = "serde_bytes")]
    pub nonce: Vec<u8>,
    /// Wrapped data key produced by the backend.
    #[serde(with = "serde_bytes")]
    pub sealed_key: Vec<u8>,
    /// Backend-specific key reference used to locate the wrapping key.
    #[serde(with = "serde_bytes")]
    pub key_ref: Vec<u8>,
    /// Optional extra AAD; currently unsupported and must be empty.
    #[serde(default, with = "serde_bytes")]
    pub aad: Vec<u8>,
    /// Optional payload length for validation and metadata.
    pub payload_len: Option<u64>,
}

/// Envelope containing the header and ciphertext bytes.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Envelope {
    /// Header fields for validation and backend selection.
    pub header: EnvelopeHeader,
    /// AEAD ciphertext bytes.
    #[serde(with = "serde_bytes")]
    pub ciphertext: Vec<u8>,
}

// AAD header excludes the `aad` field itself to avoid recursion.
#[derive(Debug, Clone, Serialize)]
struct EnvelopeHeaderAad {
    #[serde(with = "serde_bytes")]
    pub magic: Vec<u8>,
    pub version: u8,
    pub backend_id: String,
    pub wrap_algo: String,
    pub aead_algo: String,
    #[serde(with = "serde_bytes")]
    pub nonce: Vec<u8>,
    #[serde(with = "serde_bytes")]
    pub sealed_key: Vec<u8>,
    #[serde(with = "serde_bytes")]
    pub key_ref: Vec<u8>,
    pub payload_len: Option<u64>,
}

/// Format and validation errors for envelope encoding/decoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatError {
    /// CBOR serialization error with context.
    Cbor(String),
    /// Envelope magic did not match expectations.
    InvalidMagic,
    /// Unsupported envelope version encountered.
    UnsupportedVersion(u8),
    /// The AAD field is currently unsupported and must be empty.
    UnexpectedAad,
}

impl std::fmt::Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cbor(message) => write!(f, "cbor error: {message}"),
            Self::InvalidMagic => write!(f, "invalid envelope magic"),
            Self::UnsupportedVersion(version) => write!(f, "unsupported version: {version}"),
            Self::UnexpectedAad => write!(f, "unsupported envelope aad field"),
        }
    }
}

impl std::error::Error for FormatError {}

/// Construct a new envelope header with the default magic/version.
pub fn new_header(
    backend_id: impl Into<String>,
    wrap_algo: impl Into<String>,
    aead_algo: impl Into<String>,
    nonce: Vec<u8>,
    sealed_key: Vec<u8>,
    key_ref: Vec<u8>,
    payload_len: Option<u64>,
) -> EnvelopeHeader {
    EnvelopeHeader {
        magic: MAGIC.to_vec(),
        version: VERSION,
        backend_id: backend_id.into(),
        wrap_algo: wrap_algo.into(),
        aead_algo: aead_algo.into(),
        nonce,
        sealed_key,
        key_ref,
        aad: Vec::new(),
        payload_len,
    }
}

/// Serialize an envelope to CBOR bytes.
pub fn encode_envelope(envelope: &Envelope) -> Result<Vec<u8>, FormatError> {
    serde_cbor::to_vec(envelope).map_err(|err| FormatError::Cbor(err.to_string()))
}

/// Decode and validate a CBOR-encoded envelope.
pub fn decode_envelope(bytes: &[u8]) -> Result<Envelope, FormatError> {
    let envelope: Envelope =
        serde_cbor::from_slice(bytes).map_err(|err| FormatError::Cbor(err.to_string()))?;
    validate_envelope(&envelope)?;
    Ok(envelope)
}

/// Validate invariant fields in the envelope header.
pub fn validate_envelope(envelope: &Envelope) -> Result<(), FormatError> {
    if envelope.header.magic.as_slice() != MAGIC {
        return Err(FormatError::InvalidMagic);
    }
    if envelope.header.version != VERSION {
        return Err(FormatError::UnsupportedVersion(envelope.header.version));
    }
    if !envelope.header.aad.is_empty() {
        return Err(FormatError::UnexpectedAad);
    }
    Ok(())
}

/// Compute CBOR-encoded AAD bytes from a header.
pub fn aad_bytes(header: &EnvelopeHeader) -> Result<Vec<u8>, FormatError> {
    let aad_header = EnvelopeHeaderAad {
        magic: header.magic.clone(),
        version: header.version,
        backend_id: header.backend_id.clone(),
        wrap_algo: header.wrap_algo.clone(),
        aead_algo: header.aead_algo.clone(),
        nonce: header.nonce.clone(),
        sealed_key: header.sealed_key.clone(),
        key_ref: header.key_ref.clone(),
        payload_len: header.payload_len,
    };
    serde_cbor::to_vec(&aad_header).map_err(|err| FormatError::Cbor(err.to_string()))
}

#[cfg(test)]
mod tests {
    use super::{
        decode_envelope, encode_envelope, new_header, Envelope, FormatError, MAGIC, VERSION,
    };

    fn sample_envelope() -> Envelope {
        let header = new_header(
            "secure_enclave",
            "ecies-p256",
            "chacha20poly1305",
            vec![1, 2, 3],
            vec![4, 5, 6],
            vec![7, 8, 9],
            Some(42),
        );
        Envelope {
            header,
            ciphertext: vec![10, 11, 12],
        }
    }

    #[test]
    fn round_trip_envelope() {
        let envelope = sample_envelope();
        let encoded = encode_envelope(&envelope).expect("encode");
        let decoded = decode_envelope(&encoded).expect("decode");
        assert_eq!(decoded, envelope);
        assert_eq!(decoded.header.magic, MAGIC);
        assert_eq!(decoded.header.version, VERSION);
    }

    #[test]
    fn rejects_invalid_magic() {
        let mut envelope = sample_envelope();
        envelope.header.magic = b"BADMAGC".to_vec();
        let encoded = encode_envelope(&envelope).expect("encode");
        let err = decode_envelope(&encoded).expect_err("decode should fail");
        assert_eq!(err, FormatError::InvalidMagic);
    }

    #[test]
    fn rejects_unsupported_version() {
        let mut envelope = sample_envelope();
        envelope.header.version = VERSION + 1;
        let encoded = encode_envelope(&envelope).expect("encode");
        let err = decode_envelope(&encoded).expect_err("decode should fail");
        assert_eq!(err, FormatError::UnsupportedVersion(VERSION + 1));
    }

    #[test]
    fn rejects_unexpected_aad() {
        let mut envelope = sample_envelope();
        envelope.header.aad = vec![1, 2, 3];
        let encoded = encode_envelope(&envelope).expect("encode");
        let err = decode_envelope(&encoded).expect_err("decode should fail");
        assert_eq!(err, FormatError::UnexpectedAad);
    }
}
