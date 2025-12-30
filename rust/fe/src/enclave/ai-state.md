# AI State (src/enclave)

Last updated: Documented enclave backend structure and responsibilities.

## Module map
- `src/enclave/mod.rs`: Shared types (`SealedKey`), error model (`EnclaveError`), and `Registry` for backend selection.
- `src/enclave/secure_enclave.rs`: macOS Secure Enclave backend (Security.framework) with a non-macOS stub.
- `src/enclave/yubikey.rs`: YubiKey PIV RSA backend with slot/pin handling and PKCS#1 v1.5 unpadding.
- `src/enclave/mock.rs`: Deterministic mock backend for tests and development.

## Key interfaces
- `Enclave` trait: `backend_id`, `seal_key`, `unseal_key`, and `is_available` for backend selection.
- `Registry`: selects available backend or resolves explicit overrides.

## Notes
- Secure Enclave backend stores a key label in the envelope and enforces `wrap_algo` checks.
- YubiKey backend expects RSA-2048 keys and uses `FE_YUBIKEY_SLOT`/`FE_YUBIKEY_PIN` for configuration.
