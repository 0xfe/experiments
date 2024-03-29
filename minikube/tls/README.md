# Setup TLS certs

Using `cfssl` from https://github.com/cloudflare/cfssl.

Instructions: https://rob-blackbourn.medium.com/how-to-use-cfssl-to-create-self-signed-certificates-d55f76ba5781

Consider using [mkcert](https://github.com/FiloSottile/mkcert) for quick cert hacking.

## Installation

Install cfssl.

```
sudo apt install build-essential manpages-dev jq
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

### Bundle certs for installation

Generate the bundle for the webserver. The bundle consists of the entire chain of certs (excluding the root CA cert). In this case, there should be two certs in the bundle (intermediate, host.) The root cert must be manually installed in the client/browser (see below.)

```
cfssl bundle -ca-bundle ca.pem -int-bundle intermediate_ca.pem -cert ingress-host-server.pem | jq .bundle -r >ingress-host-bundle.pem
```

Another way to create the bundle:

```
cat certificates/my-webserver.pem intermediate/intermediate-ca.pem > certificates/my-webserver-fullchain.pem
```

Use the bundle file `ingress-host-bundle.pem` and the key `ingress-host-server-key.pem` to install into webserver or ingress.

### Setup private CA as a trusted authority in client/browser

Still getting warnings about this page not being secure and NET::ERR_CERT_AUTHORITY_INVALID? There’s one important piece of the puzzle missing:

Import `ca.pem` as a trusted Certificated Authority in your OS/browser! This needs to be done for every device that should trust certificates issued by the root CA and intermediaries.

For quick hacking you can use: https://github.com/FiloSottile/mkcert


### Install into k8s (not finished)

```
$ kubectl -n kube-system create secret tls ingress-host --key ingress-host-key.pem --cert ingress-host-bundle.pem
```