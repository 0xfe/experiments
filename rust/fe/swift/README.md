# Local Swift Crypto

```bash
# Init code signing certs (creates certs in keychain, requires touch)
 ../scripts/new-codesign-cert.sh

 # Sign (requires passowrd, click "Always Allow")
 ../scripts/codesign.sh fe

# Run
$ ./fe init mykey

# List codesign identities
$ security find-identity -p codesigning -v
  1) 5D1906C75F2A8FA6824DDFDBA43B4E16039B0690 "LOCALDEV001"
     1 valid identities found

# Delete our identity
$ security find-identity -p codesigning -v \
    | grep "LOCALDEV001" | awk '{print $2}' \
    | xargs -I{} security delete-certificate -Z \{\} ~/Library/Keychains/login.keychain-db
```
