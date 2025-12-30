//! Enclave backend registry, shared types, and error model.

// Secure Enclave backend (macOS).
pub mod secure_enclave;
// YubiKey PIV backend.
pub mod yubikey;
// Mock backend for tests and development.
#[cfg(any(test, feature = "mock-enclave"))]
pub mod mock;

/// Sealed data key produced by a backend and stored in the envelope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SealedKey {
    /// Backend identifier that created this sealed key.
    pub backend_id: String,
    /// Backend-specific key reference bytes.
    pub key_ref: Vec<u8>,
    /// Wrapped data key bytes.
    pub wrapped_key: Vec<u8>,
    /// Identifier for the key-wrapping algorithm.
    pub wrap_algo: String,
}

/// Errors surfaced by enclave backends and registry selection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnclaveError {
    /// Backend exists but is currently unavailable on this machine.
    Unavailable,
    /// Backend or algorithm is unsupported.
    Unsupported,
    /// Sealed key metadata is malformed or mismatched.
    InvalidSealedKey,
    /// Backend-specific error message.
    Backend(String),
    /// Requested backend identifier does not exist.
    BackendNotFound(String),
    /// Backend could not locate the referenced key.
    KeyNotFound(String),
}

impl std::fmt::Display for EnclaveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unavailable => write!(f, "backend unavailable"),
            Self::Unsupported => write!(f, "backend unsupported"),
            Self::InvalidSealedKey => write!(f, "invalid sealed key"),
            Self::Backend(message) => write!(f, "backend error: {message}"),
            Self::BackendNotFound(id) => write!(f, "backend not found: {id}"),
            Self::KeyNotFound(key_ref) => write!(f, "key not found: {key_ref}"),
        }
    }
}

impl std::error::Error for EnclaveError {}

/// Abstraction for enclave-backed key wrapping services.
pub trait Enclave {
    /// Stable identifier embedded in envelopes for backend selection.
    fn backend_id(&self) -> &'static str;
    /// Wrap a data key for storage in an envelope.
    fn seal_key(&self, data_key: &[u8]) -> Result<SealedKey, EnclaveError>;
    /// Unwrap a data key from a sealed key payload.
    fn unseal_key(&self, sealed: &SealedKey) -> Result<Vec<u8>, EnclaveError>;
    /// Indicate whether the backend is usable on this machine.
    fn is_available(&self) -> bool {
        true
    }
}

/// Collection of enclave backends with selection helpers.
pub struct Registry {
    backends: Vec<Box<dyn Enclave>>,
}

impl Registry {
    /// Create an empty registry for manual backend registration.
    pub fn new() -> Self {
        Self {
            backends: Vec::new(),
        }
    }

    /// Add a backend implementation to the registry.
    pub fn with_backend(mut self, backend: impl Enclave + 'static) -> Self {
        self.backends.push(Box::new(backend));
        self
    }

    /// Build the default registry with all compiled-in backends.
    pub fn default() -> Self {
        let registry = Self::new()
            .with_backend(secure_enclave::SecureEnclave)
            .with_backend(yubikey::YubiKeyBackend);
        #[cfg(any(test, feature = "mock-enclave"))]
        let registry = registry.with_backend(mock::MockEnclave);
        registry
    }

    /// Choose a backend for sealing, honoring an explicit override if provided.
    pub fn select_for_seal(&self, override_id: Option<&str>) -> Result<&dyn Enclave, EnclaveError> {
        if let Some(id) = override_id {
            return self.by_id(id);
        }
        self.backends
            .iter()
            .map(|backend| backend.as_ref())
            .find(|backend| backend.is_available())
            .ok_or(EnclaveError::Unavailable)
    }

    /// Locate a backend by ID and ensure it is available.
    pub fn by_id(&self, id: &str) -> Result<&dyn Enclave, EnclaveError> {
        let backend = self
            .backends
            .iter()
            .map(|backend| backend.as_ref())
            .find(|backend| backend.backend_id() == id)
            .ok_or_else(|| EnclaveError::BackendNotFound(id.to_string()))?;
        if backend.is_available() {
            Ok(backend)
        } else {
            Err(EnclaveError::Unavailable)
        }
    }

    /// List identifiers for available backends to aid error reporting.
    pub fn available_backend_ids(&self) -> Vec<&'static str> {
        self.backends
            .iter()
            .map(|backend| backend.as_ref())
            .filter(|backend| backend.is_available())
            .map(|backend| backend.backend_id())
            .collect()
    }
}
