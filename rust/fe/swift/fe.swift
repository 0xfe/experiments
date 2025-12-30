import Foundation
import Security
import CryptoKit

func die(_ msg: String) -> Never { fputs(msg + "\n", stderr); exit(1) }

func loadOrCreateKey(label: String, create: Bool) -> SecKey? {
    let tag = "sefile.\(label)".data(using: .utf8)!

    // Query existing private key
    let query: [String: Any] = [
        kSecClass as String: kSecClassKey,
        kSecAttrApplicationTag as String: tag,
        kSecAttrKeyType as String: kSecAttrKeyTypeECSECPrimeRandom,
        kSecReturnRef as String: true
    ]
    var item: CFTypeRef?
    let status = SecItemCopyMatching(query as CFDictionary, &item)
    if status == errSecSuccess, let key = item as! SecKey? { return key }

    if !create { return nil }

    // Create new Secure Enclave keypair with Touch ID gating on private-key usage
    var error: Unmanaged<CFError>?

    let access =
        SecAccessControlCreateWithFlags(
            kCFAllocatorDefault,
            kSecAttrAccessibleWhenUnlockedThisDeviceOnly,
            // Choose ONE:
            // .biometryCurrentSet => invalidates if enrolled fingerprints change
            // .biometryAny        => still works if fingerprints change
            [ .privateKeyUsage, .biometryCurrentSet ],
            &error
        )
    if access == nil { die("SecAccessControlCreateWithFlags failed: \(error!.takeRetainedValue())") }

    let attrs: [String: Any] = [
        kSecAttrKeyType as String: kSecAttrKeyTypeECSECPrimeRandom,
        kSecAttrKeySizeInBits as String: 256,
        kSecAttrTokenID as String: kSecAttrTokenIDSecureEnclave,
        kSecPrivateKeyAttrs as String: [
            kSecAttrIsPermanent as String: true,
            kSecAttrApplicationTag as String: tag,
            kSecAttrAccessControl as String: access!
        ]
    ]

    guard let priv = SecKeyCreateRandomKey(attrs as CFDictionary, &error) else {
        die("SecKeyCreateRandomKey failed: \(error!.takeRetainedValue())")
    }
    return priv
}

func publicKey(from privateKey: SecKey) -> SecKey {
    guard let pub = SecKeyCopyPublicKey(privateKey) else { die("SecKeyCopyPublicKey failed") }
    return pub
}

// File format:
// [4B magic "SEF1"]
// [2B wrappedKeyLen BE][wrappedKey]
// [1B nonceLen][nonce]   (AES-GCM nonce = 12 bytes)
// [2B tagLen BE][tag]    (AES-GCM tag = 16 bytes)
// [ciphertext...]
let magic = Data([0x53,0x45,0x46,0x31]) // "SEF1"

func be16(_ n: Int) -> Data { Data([UInt8((n >> 8) & 0xff), UInt8(n & 0xff)]) }
func readBE16(_ d: Data, _ i: inout Int) -> Int {
    let v = (Int(d[i]) << 8) | Int(d[i+1]); i += 2; return v
}

func encryptFile(label: String, input: URL, output: URL) throws {
    guard let priv = loadOrCreateKey(label: label, create: false) else {
        die("No key for label '\(label)'. Run: sefile init \(label)")
    }
    let pub = publicKey(from: priv)

    // 1) random AES-256 key
    let symKey = SymmetricKey(size: .bits256)
    let symKeyData = symKey.withUnsafeBytes { Data($0) }

    // 2) AES-GCM encrypt file
    let plaintext = try Data(contentsOf: input)
    let sealed = try AES.GCM.seal(plaintext, using: symKey)
    let nonce = sealed.nonce.withUnsafeBytes { Data($0) }
    let tag = sealed.tag

    // 3) wrap AES key using public key (ECIES)
    var error: Unmanaged<CFError>?
    let alg = SecKeyAlgorithm.eciesEncryptionCofactorX963SHA256AESGCM
    guard SecKeyIsAlgorithmSupported(pub, .encrypt, alg) else { die("ECIES algorithm not supported") }
    guard let wrapped = SecKeyCreateEncryptedData(pub, alg, symKeyData as CFData, &error) as Data? else {
        die("SecKeyCreateEncryptedData failed: \(error!.takeRetainedValue())")
    }

    var out = Data()
    out.append(magic)
    out.append(be16(wrapped.count)); out.append(wrapped)
    out.append(UInt8(nonce.count)); out.append(nonce)
    out.append(be16(tag.count)); out.append(tag)
    out.append(sealed.ciphertext)
    try out.write(to: output, options: .atomic)
}

func decryptFile(label: String, input: URL, output: URL) throws {
    guard let priv = loadOrCreateKey(label: label, create: false) else {
        die("No key for label '\(label)'.")
    }

    let blob = try Data(contentsOf: input)
    var i = 0
    guard blob.count > 4, blob.prefix(4) == magic else { die("Bad header (not SEF1)") }
    i += 4

    let wrappedLen = readBE16(blob, &i)
    let wrapped = blob.subdata(in: i..<(i+wrappedLen)); i += wrappedLen

    let nonceLen = Int(blob[i]); i += 1
    let nonceData = blob.subdata(in: i..<(i+nonceLen)); i += nonceLen

    let tagLen = readBE16(blob, &i)
    let tag = blob.subdata(in: i..<(i+tagLen)); i += tagLen

    let ciphertext = blob.suffix(from: i)

    // 1) unwrap AES key using private key -> THIS triggers Touch ID prompt
    var error: Unmanaged<CFError>?
    let alg = SecKeyAlgorithm.eciesEncryptionCofactorX963SHA256AESGCM
    guard SecKeyIsAlgorithmSupported(priv, .decrypt, alg) else { die("ECIES decrypt not supported") }
    guard let symKeyData = SecKeyCreateDecryptedData(priv, alg, wrapped as CFData, &error) as Data? else {
        die("SecKeyCreateDecryptedData failed: \(error!.takeRetainedValue())")
    }

    let symKey = SymmetricKey(data: symKeyData)

    // 2) AES-GCM decrypt
    let nonce = try AES.GCM.Nonce(data: nonceData)
    let sealedBox = try AES.GCM.SealedBox(nonce: nonce, ciphertext: ciphertext, tag: tag)
    let plaintext = try AES.GCM.open(sealedBox, using: symKey)
    try plaintext.write(to: output, options: .atomic)
}

let args = CommandLine.arguments
if args.count < 2 { die("Usage: sefile init|enc|dec <label> [in] [out]") }

let cmd = args[1]
switch cmd {
case "init":
    guard args.count == 3 else { die("Usage: sefile init <label>") }
    _ = loadOrCreateKey(label: args[2], create: true)
    print("OK: created/loaded key '\(args[2])'")
case "enc":
    guard args.count == 5 else { die("Usage: sefile enc <label> <in> <out>") }
    try encryptFile(label: args[2], input: URL(fileURLWithPath: args[3]), output: URL(fileURLWithPath: args[4]))
case "dec":
    guard args.count == 5 else { die("Usage: sefile dec <label> <in> <out>") }
    try decryptFile(label: args[2], input: URL(fileURLWithPath: args[3]), output: URL(fileURLWithPath: args[4]))
default:
    die("Unknown command \(cmd)")
}
