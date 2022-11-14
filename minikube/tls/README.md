# Setup TLS certs

Using `cfssl` from https://github.com/cloudflare/cfssl.

Instructions: https://rob-blackbourn.medium.com/how-to-use-cfssl-to-create-self-signed-certificates-d55f76ba5781

## Installation

Install cfssl.

```
sudo apt install build-essential manpages-dev
go install github.com/cloudflare/cfssl/cmd/cfssl
go install github.com/cloudflare/cfssl/cmd/cfssljson
```

### Quick Gen Certs

Edit `ingress_host.json` and setup host details, then run:

```
cfssl gencert -initca ca.json | cfssljson -bare ca
cfssl gencert -initca intermediate-ca.json | cfssljson -bare intermediate_ca
cfssl sign -ca ca.pem -ca-key ca-key.pem -config cfssl.json -profile intermediate_ca intermediate_ca.csr | cfssljson -bare intermediate_ca
cfssl gencert -ca intermediate_ca.pem -ca-key intermediate_ca-key.pem -config cfssl.json -profile=server ingress_host.json | cfssljson -bare ingress-host-server
```

## Create Certificates

### Create Root CA

Edit `ca.json` and run:

```
# generates ca.csr, ca.pem and ca-key.pem
cfssl gencert -initca ca.json | cfssljson -bare ca
```

- `ca.csr` - cert signing request
- `ca.pem` - ca cert
- `ca-key.pem` - ca private key (note file perms: private)

### Create Profile Configuration

The profile specifies general attributes for different types of certs, e.g., profiles for intermediate CAs or host certs. Edit `cfssl.json` to modify the profile.

#### Profiles

- `intermediate_ca` -- has `is_ca` constraint enabled.
- `server` -- has `server auth` but no `client auth`.
- `client` -- has `client auth` but no `server auth`.
- `peer` -- has `server auth` *and* `client auth`.

### Create Intermediate CA

Create the intermediate CA's certs and sign with root CA. Edit `intermediate-ca.json` and run:

```
# Create intermediate CA
cfssl gencert -initca intermediate-ca.json | cfssljson -bare intermediate_ca

# Sign the intermediate CA's CSR with the root CA's key using the intermediate_ca profile
cfssl sign -ca ca.pem -ca-key ca-key.pem -config cfssl.json -profile intermediate_ca intermediate_ca.csr | cfssljson -bare intermediate_ca
```

- `intermediate_ca.csr` - cert signing request
- `intermediate_ca.pem` - ca cert
- `intermediate_ca-key.pem` - ca private key (note file perms: private)

### Create Host Certs

Use `k get ingress` to get ingress IP address, then edit `ingress_host.json`. Typically we require only server certificates.

```
# Generate server certs
cfssl gencert -ca intermediate_ca.pem -ca-key intermediate_ca-key.pem -config cfssl.json -profile=server ingress_host.json | cfssljson -bare ingress-host-server

# Generate client certificate (optional)
cfssl gencert -ca intermediate_ca.pem -ca-key intermediate_ca-key.pem -config cfssl.json -profile=client ingress_host.json | cfssljson -bare ingress-host-client

# Generate peer certificate (optional)
cfssl gencert -ca intermediate_ca.pem -ca-key intermediate_ca-key.pem -config cfssl.json -profile=peer ingress_host.json | cfssljson -bare ingress-host-peer
```

