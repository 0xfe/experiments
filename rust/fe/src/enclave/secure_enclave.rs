//! Secure Enclave backend implementation with macOS and non-macOS variants.

// Shared enclave trait and types used by all backends.
use super::{Enclave, EnclaveError, SealedKey};

/// Zero-sized marker type used to select the Secure Enclave backend.
#[derive(Debug, Default, Clone, Copy)]
pub struct SecureEnclave;

impl SecureEnclave {
    /// Compile-time guard so we never attempt Secure Enclave calls off macOS.
    pub fn is_supported_platform() -> bool {
        cfg!(target_os = "macos")
    }
}

impl Enclave for SecureEnclave {
    fn backend_id(&self) -> &'static str {
        // Stable identifier embedded in sealed envelopes.
        "secure_enclave"
    }

    fn seal_key(&self, data_key: &[u8]) -> Result<SealedKey, EnclaveError> {
        // Delegate to the OS-specific implementation.
        platform::seal_key(data_key)
    }

    fn unseal_key(&self, sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError> {
        // Delegate to the OS-specific implementation.
        platform::unseal_key(sealed)
    }

    fn is_available(&self) -> bool {
        // Delegate to the OS-specific implementation.
        platform::is_available()
    }
}

#[cfg(target_os = "macos")]
// macOS Secure Enclave implementation using Security.framework.
mod platform {
    use super::{EnclaveError, SealedKey};
    // CoreFoundation bridging types for SecKey and CFError interop.
    use core_foundation::base::TCFType;
    use core_foundation::data::CFData;
    use core_foundation::error::CFError;
    use core_foundation_sys::error::CFErrorRef;
    // Security.framework access control primitives for keychain items.
    use security_framework::access_control::{ProtectionMode, SecAccessControl};
    // Keychain queries to locate an existing Secure Enclave key.
    use security_framework::item::{
        ItemClass, ItemSearchOptions, KeyClass, Location, Reference, SearchResult,
    };
    // SecKey APIs for key generation and ECIES operations.
    use security_framework::key::{Algorithm, GenerateKeyOptions, KeyType, SecKey, Token};
    // Access control flag for private key usage only.
    use security_framework::passwords_options::AccessControlOptions;
    use security_framework_sys::base::errSecItemNotFound;
    use security_framework_sys::key::{
        SecKeyCreateDecryptedData, SecKeyCreateEncryptedData, SecKeyIsAlgorithmSupported,
        kSecKeyOperationTypeDecrypt, kSecKeyOperationTypeEncrypt,
    };
    // Used to pass nullable CFError out-params to C APIs.
    use std::ptr;

    /// Keychain label for the Secure Enclave private key we manage.
    const KEY_LABEL: &str = "fe-secure-enclave-key";
    /// Stored in the envelope so we can reject mismatched or future algorithms.
    const WRAP_ALGO: &str = "ecies-x963-sha256-aesgcm";
    /// Secure Enclave supports P-256; 256-bit EC key is required for ECIES here.
    const KEY_SIZE_BITS: u32 = 256;
    /// OSStatus used when Keychain entitlements are missing.
    const ERR_SEC_MISSING_ENTITLEMENT: i64 = -34018;

    // Fast probe to decide if the Secure Enclave API can be used on this device.
    pub(super) fn is_available() -> bool {
        // Access-control creation is a lightweight proxy for Secure Enclave availability.
        create_access_control().is_ok()
    }

    // Wrap a data key using the Secure Enclave public key; returns a sealed envelope payload.
    pub(super) fn seal_key(data_key: &[u8]) -> Result<SealedKey, EnclaveError> {
        // The private key never leaves the enclave; we encrypt using the public half.
        let key = get_or_create_key()?;
        let public = key
            .public_key()
            .ok_or_else(|| EnclaveError::Backend("missing public key".to_string()))?;
        // ECIES with AES-GCM provides authenticated encryption for the data key.
        let algorithm = Algorithm::ECIESEncryptionStandardX963SHA256AESGCM;
        // Some keys cannot perform every algorithm; check first for clear errors.
        ensure_supported(&public, kSecKeyOperationTypeEncrypt, algorithm)?;
        let encrypted = encrypt(&public, algorithm, data_key)?;
        Ok(SealedKey {
            backend_id: "secure_enclave".to_string(),
            // We persist the key label so we can locate the correct key on unseal.
            key_ref: KEY_LABEL.as_bytes().to_vec(),
            wrapped_key: encrypted,
            wrap_algo: WRAP_ALGO.to_string(),
        })
    }

    // Unwrap a previously sealed key using the Secure Enclave private key.
    pub(super) fn unseal_key(sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError> {
        // Prevent mismatched or downgraded algorithms.
        if sealed.wrap_algo != WRAP_ALGO {
            return Err(EnclaveError::InvalidSealedKey);
        }
        // The key label is stored as bytes in the envelope for portability.
        let label =
            std::str::from_utf8(&sealed.key_ref).map_err(|_| EnclaveError::InvalidSealedKey)?;
        let key = get_or_create_key_with_label(label)?;
        let algorithm = Algorithm::ECIESEncryptionStandardX963SHA256AESGCM;
        // Ensure the private key supports decrypt with the chosen algorithm.
        ensure_supported(&key, kSecKeyOperationTypeDecrypt, algorithm)?;
        decrypt(&key, algorithm, &sealed.wrapped_key)
    }

    // Default to the canonical key label used by this backend.
    fn get_or_create_key() -> Result<SecKey, EnclaveError> {
        get_or_create_key_with_label(KEY_LABEL)
    }

    // Locate the key by label or create it if it is the canonical label.
    fn get_or_create_key_with_label(label: &str) -> Result<SecKey, EnclaveError> {
        // Only allow key creation for the canonical label; other labels must exist.
        if label != KEY_LABEL {
            return find_key(label);
        }
        match find_key(label) {
            Ok(key) => Ok(key),
            Err(EnclaveError::KeyNotFound(_)) => create_key(label),
            Err(err) => Err(err),
        }
    }

    // Query the keychain for a private key with the provided label.
    fn find_key(label: &str) -> Result<SecKey, EnclaveError> {
        let results = ItemSearchOptions::new()
            .class(ItemClass::key())
            .key_class(KeyClass::private())
            .label(label)
            .load_refs(true)
            .search();
        let results = match results {
            Ok(results) => results,
            Err(err) if err.code() == errSecItemNotFound => {
                // Distinguish missing key from other Keychain failures.
                return Err(EnclaveError::KeyNotFound(label.to_string()));
            }
            Err(err) => return Err(EnclaveError::Backend(err.to_string())),
        };
        for result in results {
            if let SearchResult::Ref(Reference::Key(key)) = result {
                return Ok(key);
            }
        }
        Err(EnclaveError::KeyNotFound(label.to_string()))
    }

    // Generate a new Secure Enclave-backed EC key with strict access control.
    fn create_key(label: &str) -> Result<SecKey, EnclaveError> {
        let access_control = create_access_control().map_err(EnclaveError::Backend)?;
        let mut options = GenerateKeyOptions::default();
        options
            .set_key_type(KeyType::ec())
            .set_size_in_bits(KEY_SIZE_BITS)
            .set_label(label)
            .set_token(Token::SecureEnclave)
            // Store a keychain reference; private key material remains in the enclave.
            .set_access_control(access_control)
            .set_location(secure_enclave_location());
        SecKey::generate(options.to_dictionary())
            .map_err(|err| map_cf_error(err, "secure enclave key generation"))
    }

    // Configure key access so it is only usable when the device is unlocked.
    fn create_access_control() -> Result<SecAccessControl, String> {
        // Only permit private key usage, and only when the device is unlocked.
        let flags = AccessControlOptions::PRIVATE_KEY_USAGE.bits();
        SecAccessControl::create_with_protection(
            Some(ProtectionMode::AccessibleWhenUnlockedThisDeviceOnly),
            flags,
        )
        .map_err(|err| err.to_string())
    }

    fn ensure_supported(
        key: &SecKey,
        operation: u32,
        algorithm: Algorithm,
    ) -> Result<(), EnclaveError> {
        // SecKey operations are key- and algorithm-specific; check to avoid runtime failures.
        let supported = unsafe {
            SecKeyIsAlgorithmSupported(key.as_concrete_TypeRef(), operation, algorithm.into())
        };
        if supported == 0 {
            return Err(EnclaveError::Unsupported);
        }
        Ok(())
    }

    fn encrypt(
        key: &SecKey,
        algorithm: Algorithm,
        plaintext: &[u8],
    ) -> Result<Vec<u8>, EnclaveError> {
        // Security.framework uses CFData and returns unmanaged references with CFError out-params.
        let mut error = ptr::null_mut();
        let data = CFData::from_buffer(plaintext);
        let encrypted = unsafe {
            SecKeyCreateEncryptedData(
                key.as_concrete_TypeRef(),
                algorithm.into(),
                data.as_concrete_TypeRef(),
                &mut error,
            )
        };
        if !error.is_null() {
            return Err(EnclaveError::Backend(cf_error_to_string(
                error,
                "secure enclave encryption failed",
            )));
        }
        if encrypted.is_null() {
            return Err(EnclaveError::Backend(
                "secure enclave encryption failed".to_string(),
            ));
        }
        // Take ownership of the CFData returned by Create rule API.
        let data = unsafe { CFData::wrap_under_create_rule(encrypted) };
        Ok(data.to_vec())
    }

    fn decrypt(
        key: &SecKey,
        algorithm: Algorithm,
        ciphertext: &[u8],
    ) -> Result<Vec<u8>, EnclaveError> {
        // Mirror the encrypt flow, but using the private key in the enclave.
        let mut error = ptr::null_mut();
        let data = CFData::from_buffer(ciphertext);
        let decrypted = unsafe {
            SecKeyCreateDecryptedData(
                key.as_concrete_TypeRef(),
                algorithm.into(),
                data.as_concrete_TypeRef(),
                &mut error,
            )
        };
        if !error.is_null() {
            return Err(EnclaveError::Backend(cf_error_to_string(
                error,
                "secure enclave decryption failed",
            )));
        }
        if decrypted.is_null() {
            return Err(EnclaveError::Backend(
                "secure enclave decryption failed".to_string(),
            ));
        }
        // Take ownership of the CFData returned by Create rule API.
        let data = unsafe { CFData::wrap_under_create_rule(decrypted) };
        Ok(data.to_vec())
    }

    // Convert a CoreFoundation error to a human-friendly string for diagnostics.
    fn cf_error_to_string(error: CFErrorRef, context: &str) -> String {
        // Handle null defensively; CFErrorRef is opaque.
        if error.is_null() {
            return context.to_string();
        }
        let error = unsafe { CFError::wrap_under_create_rule(error) };
        let code = error.code() as i64;
        let desc = error.description().to_string();
        if code == ERR_SEC_MISSING_ENTITLEMENT {
            return format!("{context}: missing Keychain entitlements (OSStatus {code})");
        }
        format!("{context}: {desc} (OSStatus {code})")
    }

    fn map_cf_error(error: CFError, context: &str) -> EnclaveError {
        let code = error.code() as i64;
        let desc = error.description().to_string();
        if code == ERR_SEC_MISSING_ENTITLEMENT {
            return EnclaveError::Backend(format!(
                "{context}: missing Keychain entitlements (OSStatus {code})"
            ));
        }
        EnclaveError::Backend(format!("{context}: {desc} (OSStatus {code})"))
    }

    fn secure_enclave_location() -> Location {
        Location::DataProtectionKeychain
    }
}

#[cfg(not(target_os = "macos"))]
// Non-macOS implementation that cleanly reports unavailability.
mod platform {
    use super::{EnclaveError, SealedKey};

    // Explicitly report that Secure Enclave support does not exist here.
    pub(super) fn is_available() -> bool {
        // Non-macOS builds never expose Secure Enclave support.
        false
    }

    // Explicitly fail so the caller can select another backend.
    pub(super) fn seal_key(_data_key: &[u8]) -> Result<SealedKey, EnclaveError> {
        // Explicit failure so callers can fall back to other backends.
        Err(EnclaveError::Unavailable)
    }

    // Explicitly fail so the caller can select another backend.
    pub(super) fn unseal_key(_sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError> {
        // Explicit failure so callers can fall back to other backends.
        Err(EnclaveError::Unavailable)
    }
}
