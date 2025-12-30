//! YubiKey PIV backend for wrapping data keys with RSA.

use super::{Enclave, EnclaveError, SealedKey};

/// Backend wrapper for YubiKey PIV RSA sealing/unsealing.
#[derive(Debug, Default, Clone, Copy)]
pub struct YubiKeyBackend;

impl Enclave for YubiKeyBackend {
    /// Stable backend identifier for envelopes.
    fn backend_id(&self) -> &'static str {
        "yubikey"
    }

    /// Encrypt a data key using the YubiKey's public key in the selected slot.
    fn seal_key(&self, data_key: &[u8]) -> Result<SealedKey, EnclaveError> {
        let slot = slot_from_env()?;
        let mut yubikey = open_yubikey()?;
        let public_key = read_public_key(&mut yubikey, slot)?;
        let wrapped_key = encrypt_key(&public_key, data_key)?;
        Ok(SealedKey {
            backend_id: self.backend_id().to_string(),
            key_ref: format!("piv:{slot}").into_bytes(),
            wrapped_key,
            wrap_algo: WRAP_ALGO.to_string(),
        })
    }

    /// Decrypt a sealed data key using the YubiKey's private key.
    fn unseal_key(&self, sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError> {
        if sealed.wrap_algo != WRAP_ALGO {
            return Err(EnclaveError::InvalidSealedKey);
        }
        let slot = slot_from_key_ref(&sealed.key_ref)?;
        let pin = yubikey_pin()?;
        let mut yubikey = open_yubikey()?;
        yubikey
            .verify_pin(pin.as_bytes())
            .map_err(map_yubikey_error)?;
        let decrypted = decrypt_key(&mut yubikey, slot, &sealed.wrapped_key)?;
        unwrap_decrypted_key(&decrypted)
    }

    fn is_available(&self) -> bool {
        open_yubikey().is_ok()
    }
}

/// Algorithm identifier persisted in envelopes for YubiKey wrapping.
const WRAP_ALGO: &str = "piv-rsa-pkcs1v15";
/// Default PIV slot used when FE_YUBIKEY_SLOT is not set.
const DEFAULT_SLOT: &str = "9d";
/// Expected data key length for unwrapped AEAD keys.
const DATA_KEY_LEN: usize = 32;

/// Open the first attached YubiKey device.
fn open_yubikey() -> Result<yubikey::YubiKey, EnclaveError> {
    let yubikey = yubikey::YubiKey::open().map_err(map_yubikey_error)?;
    Ok(yubikey)
}

/// Read and validate the RSA public key from the YubiKey's slot certificate.
fn read_public_key(
    yubikey: &mut yubikey::YubiKey,
    slot: yubikey::piv::SlotId,
) -> Result<rsa::RsaPublicKey, EnclaveError> {
    use rsa::pkcs8::DecodePublicKey;
    use rsa::traits::PublicKeyParts;
    use x509_cert::der::Encode;

    let cert = yubikey::certificate::Certificate::read(yubikey, slot).map_err(map_yubikey_error)?;
    let spki = cert.subject_pki().to_der().map_err(|_| {
        EnclaveError::Backend("failed to encode YubiKey certificate SPKI".to_string())
    })?;
    let public_key = rsa::RsaPublicKey::from_public_key_der(&spki)
        .map_err(|_| EnclaveError::Backend("YubiKey certificate is not RSA".to_string()))?;
    if public_key.n().bits() != 2048 {
        return Err(EnclaveError::Backend(
            "YubiKey RSA key must be 2048-bit".to_string(),
        ));
    }
    Ok(public_key)
}

/// Encrypt the data key with PKCS#1 v1.5 padding for the YubiKey.
fn encrypt_key(public_key: &rsa::RsaPublicKey, data_key: &[u8]) -> Result<Vec<u8>, EnclaveError> {
    use rsa::Pkcs1v15Encrypt;
    use rsa::rand_core::OsRng;

    public_key
        .encrypt(&mut OsRng, Pkcs1v15Encrypt, data_key)
        .map_err(|err| EnclaveError::Backend(format!("RSA encrypt failed: {err}")))
}

/// Decrypt the wrapped key using the YubiKey's private key.
fn decrypt_key(
    yubikey: &mut yubikey::YubiKey,
    slot: yubikey::piv::SlotId,
    ciphertext: &[u8],
) -> Result<Vec<u8>, EnclaveError> {
    use yubikey::piv::{AlgorithmId, decrypt_data};

    let plaintext =
        decrypt_data(yubikey, ciphertext, AlgorithmId::Rsa2048, slot).map_err(map_yubikey_error)?;
    Ok(plaintext.to_vec())
}

/// Unwrap the decrypted data key, handling raw and PKCS#1 v1.5 padded formats.
fn unwrap_decrypted_key(decrypted: &[u8]) -> Result<Vec<u8>, EnclaveError> {
    if decrypted.len() == DATA_KEY_LEN {
        return Ok(decrypted.to_vec());
    }
    if let Some(unpadded) = strip_pkcs1v15(decrypted)
        && unpadded.len() == DATA_KEY_LEN
    {
        return Ok(unpadded);
    }
    Err(EnclaveError::Backend(
        "unexpected decrypted key length".to_string(),
    ))
}

/// Strip PKCS#1 v1.5 padding and return the payload bytes, if present.
fn strip_pkcs1v15(block: &[u8]) -> Option<Vec<u8>> {
    if block.len() < 11 || block[0] != 0x00 || block[1] != 0x02 {
        return None;
    }
    let mut index = 2;
    while index < block.len() && block[index] != 0x00 {
        index += 1;
    }
    if index < 10 || index >= block.len() {
        return None;
    }
    Some(block[index + 1..].to_vec())
}

/// Fetch the required YubiKey PIN from FE_YUBIKEY_PIN.
fn yubikey_pin() -> Result<String, EnclaveError> {
    std::env::var("FE_YUBIKEY_PIN").map_err(|_| {
        EnclaveError::Backend("FE_YUBIKEY_PIN is required for YubiKey unseal".to_string())
    })
}

/// Parse the YubiKey slot from FE_YUBIKEY_SLOT or use the default.
fn slot_from_env() -> Result<yubikey::piv::SlotId, EnclaveError> {
    let slot = std::env::var("FE_YUBIKEY_SLOT").unwrap_or_else(|_| DEFAULT_SLOT.to_string());
    slot.parse()
        .map_err(|_| EnclaveError::Backend("invalid FE_YUBIKEY_SLOT value".to_string()))
}

/// Parse the PIV slot identifier from the sealed key reference.
fn slot_from_key_ref(key_ref: &[u8]) -> Result<yubikey::piv::SlotId, EnclaveError> {
    let key_ref = std::str::from_utf8(key_ref).map_err(|_| EnclaveError::InvalidSealedKey)?;
    let slot = key_ref
        .strip_prefix("piv:")
        .ok_or(EnclaveError::InvalidSealedKey)?;
    slot.parse().map_err(|_| EnclaveError::InvalidSealedKey)
}

/// Normalize YubiKey library errors into enclave errors.
fn map_yubikey_error(err: yubikey::Error) -> EnclaveError {
    match err {
        yubikey::Error::NotFound => EnclaveError::Unavailable,
        yubikey::Error::WrongPin { tries } => {
            EnclaveError::Backend(format!("YubiKey PIN incorrect (tries remaining: {tries})"))
        }
        yubikey::Error::PinLocked => EnclaveError::Backend("YubiKey PIN is locked".to_string()),
        _ => EnclaveError::Backend(err.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::strip_pkcs1v15;

    #[test]
    fn strips_pkcs1_v15_padding() {
        let mut block = vec![0x00, 0x02];
        block.extend(vec![0x01; 10]);
        block.push(0x00);
        block.extend(b"data-key");
        let unpadded = strip_pkcs1v15(&block).expect("unpadded");
        assert_eq!(unpadded, b"data-key");
    }
}
