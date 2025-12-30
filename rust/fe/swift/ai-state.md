# AI State (swift)

Last updated: Captured Swift Secure Enclave CLI context.

## Files
- `swift/fe.swift`: Swift prototype CLI that generates Secure Enclave keys, seals/unseals files with AES-GCM, and stores a wrapped data key using ECIES.
- `swift/fe`: Built binary (not edited).
- `swift/hello.txt`: Sample input file for local testing.
- `swift/justfile`: Justfile with local automation targets.

## Notes
- Secure Enclave key creation requires keychain entitlements; missing entitlements surface as OSStatus -34018.
- The file format is a custom binary header with magic, wrapped key, AES-GCM nonce/tag, and ciphertext.
