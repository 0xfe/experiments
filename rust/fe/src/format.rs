use serde::{Deserialize, Serialize};

pub const MAGIC: &[u8; 7] = b"FESEAL1";
pub const VERSION: u8 = 1;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvelopeHeader {
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
    #[serde(default, with = "serde_bytes")]
    pub aad: Vec<u8>,
    pub payload_len: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Envelope {
    pub header: EnvelopeHeader,
    #[serde(with = "serde_bytes")]
    pub ciphertext: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatError {
    Cbor(String),
    InvalidMagic,
    UnsupportedVersion(u8),
}

impl std::fmt::Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cbor(message) => write!(f, "cbor error: {message}"),
            Self::InvalidMagic => write!(f, "invalid envelope magic"),
            Self::UnsupportedVersion(version) => write!(f, "unsupported version: {version}"),
        }
    }
}

impl std::error::Error for FormatError {}

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

pub fn encode_envelope(envelope: &Envelope) -> Result<Vec<u8>, FormatError> {
    serde_cbor::to_vec(envelope).map_err(|err| FormatError::Cbor(err.to_string()))
}

pub fn decode_envelope(bytes: &[u8]) -> Result<Envelope, FormatError> {
    let envelope: Envelope =
        serde_cbor::from_slice(bytes).map_err(|err| FormatError::Cbor(err.to_string()))?;
    validate_envelope(&envelope)?;
    Ok(envelope)
}

pub fn validate_envelope(envelope: &Envelope) -> Result<(), FormatError> {
    if envelope.header.magic.as_slice() != MAGIC {
        return Err(FormatError::InvalidMagic);
    }
    if envelope.header.version != VERSION {
        return Err(FormatError::UnsupportedVersion(envelope.header.version));
    }
    Ok(())
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
}
