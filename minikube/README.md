# minikube / k3s experiment

This repo implements an end-to-end web and commandline "diceroll app" with a gRPC backend running on k8s. Uses:

### Shared
- Go 1.19
- gRPC / Protobuf
- Envoy for gRPC LB
- cfssl for CA cert management
- Helm for templatized k8s configs
- NFS for persistent volumes (mounted on external server)

### Minikube
- Minikube
- Docker
- nginx Ingress (on minikube)
- iptables rules for exposing minikube bridge network to the outside world

### k3s
- k3s
- containerd (on k3s, no Docker)
  + but images built with Docker
- Google Artifact Registry
- Traefik Ingress (on pikube)

### Subdirectories

- `tls/` - Configs to generate TLS certs for root and intermediate CAs, and ingress
- `helm/` - Helm templates for kuberenetes configs (deployments, ingress, envoy)
= `dice/` - Protobuf schemas for Dice server
- `server/` - gRPC server binary. Built with `server.Dockerfile`.
- `client/` - gRPC client library.
  - `client/cmd` - Commandline tool that uses the client library to talk to the server
- `main.go` - Web server that talks to the gRPC server. Exposes `/roll` and `/getrolls`
- `k8s/` (deprecated) - Kuberenetes configs (deployments, ingress, envoy)

## Quick Run

```
# server
cd server
go run main.go

# client
cd client/cmd
go run main.go --target 3001

# webserver
cd {project root}
go run main.go
curl localhost:3000/roll
curl localhost:3000/getrolls

# grpcurl
$ grpcurl -plaintext localhost:3001 describe
$ grpcurl -plaintext -d '{"roller_handle": "0xfe"}' localhost:3001 RollService/Roll
```

### Quick Run with Helm

Check `values.yaml` in `helm/dice` -- make sure your ingress hostnames are resolvable.

#### On k3s

```
# Start on pikube
helm install dice helm/dice -f helm/k3s.yaml

# Test
# k get ingress
$ curl pi0.dice.pikube/roll
$ curl pi1.dice.pikube/roll

# Status / uninstall
helm list
helm status dice
helm uninstall dice

# Test it in a new temp container
k run temppod --image=debian -it
apt update && apt install net-tools curl iproute2
curl main-sevice:3000/roll
```

#### On minkube

```
# Make sure minikube is running (once only)
minikube start
eval $(minikube -p minikube docker-env)
minikube addons enable ingress

# Start on minikube (make sure you've built and pushed the container images first)
helm install dice helm/dice -f helm/minikube.yaml
k get pods

# Get ingress IP address (takes about a minute)
kubectl get ingress

# Test
$ curl 192.168.49.2/roll

# note: the backend is loadbalanced, so the response depends on which backend is hit
$ curl 192.168.49.2/getrolls
```

### Updating k8s configs in helm charts

- Add or change configs in `helm/dice/templates`
- Update the version in `helm/dice/Chart.yaml`.
- Then run:

```
$ helm upgrade dice helm/dice -f helm/k3s.yaml
```

### k3s vs minikube

- k3s images are hosted on Google Artifact Registry (minikube images are on local docker)
  + `ImagePullPolicy` is not `never` here.
- k3s ingress controller is Traefik (unlike nginx for minikube)
- pikube is a cluster or Raspberry Pis, so images must be target arm64
- ingress exposes ports on the nodes (like a real k8s cluster)
  + Minikube builds a separate internal network, so you need IP forwarding setup

## Debug

```
# run a fresh debian image inside the network
$ k run temppod --image=debian -it
temppod# cat /etc/debian_version
temppod# apt update && apt install net-tools curl iproute2
temppod# curl main-sevice:3000/roll

# Restart deployment
kubectl rollout restart deployment envoy-deployment

# Change configmap
k delete configmap envoy-conf
k create configmap envoy-conf --from-file=./k8s/config/envoy.yaml
kubectl rollout restart deployment envoy-deployment

# Run shell in new container (first time)
$ k run temppod --image=radial/busyboxplus:curl -it

# Reattach to temppod
$ k attach temppod -it

# Run shell in temppod
$ k exec temppod -it -- /bin/ash

# Delete temppod
$ k delete pod temppod

# Run shell in existing pod
$ k get pods
$ k exec -it podname -- /bin/ash
/ # nslookup main-service

# Run shell on node
$ k get node
$ kubectl debug node/minikube -it --image=ubuntu

# Logs
$ k logs podname
$ k top pod
$ k top node

# see k3s logs
sudo journalctl -u k3s.service
```
## Build

### Build container image

Minikube runs its own dockerd and container registry. If you're building to run without minikube then:

```
# build protos
cd dice
protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative dice.proto

cd ..

# builds and pushes to local repo
docker build . -f server.Dockerfile -t 0xfe/dice/server
docker build . -f Dockerfile -t 0xfe/dice/main
```

### Build container images for pikube (arm64) and push to GAR

```
# push to Google artifact registry

$ gcloud auth configure-docker northamerica-northeast2-docker.pkg.dev
$ docker build . -f server.arm64.Dockerfile -t northamerica-northeast2-docker.pkg.dev/pikube-369400/k3s/dice/server.arm64
$ docker push northamerica-northeast2-docker.pkg.dev/pikube-369400/k3s/dice/server.arm64

$ docker build . -f main.arm64.Dockerfile -t northamerica-northeast2-docker.pkg.dev/pikube-369400/k3s/dice/main.arm64
$ docker push northamerica-northeast2-docker.pkg.dev/pikube-369400/k3s/dice/main.arm64
```

If pushing to minikube, set the `DOCKER_*` env variables to point to the minikube docker, then build the images again as above.

```
eval $(minikube -p minikube docker-env)
```

### Run container on host

Run these in a separate shell that's connected to the local docker daemon, not the minkube one.

```
docker run -p 3001:3001 0xfe/dice/server
docker run -e DICE_GRPC_TARGET=localhost:3001 -p 3000:3000 0xfe/dice/main

netstat -nap | grep LIST | grep tcp
```

If you don't see ports on `netstat`, then unset DOCKER_* env variables.

### Install Protobuf and gRPC dependencies

If you change the proto file, or this is your first build, then generate the protobuf stubs.

```
# Install dependencies
$ apt install -y protobuf-compiler
$ protoc --version  # Ensure compiler version is 3+

# Install proto and grpc for Go
$ go get -u google.golang.org/protobuf/cmd/protoc-gen-go
$ go install google.golang.org/protobuf/cmd/protoc-gen-go
$ go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
$ go get google.golang.org/grpc

# grpcurl utility
$ go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest

# Build protos
$ protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative dice.proto

# Run server
$ go run server/main.go
```

## Networking

### Setup TLS certs

See [tls/README.md](https://github.com/0xfe/experiments/tree/master/minikube/tls/README.md) for details.

### Setup NFS server on nuc

```
$ sudo apt install nfs-kernel-server
$ sudo mkdir -p /opt/nfs_share
$ sudo chown -R nobody:nogroup /opt/nfs_share/
$ sudo chmod 777 /mnt/nfs_share/

$ sudo vi /etc/exports
/opt/nfs_share 192.168.16.1/24(rw,sync,no_subtree_check)

$ sudo exportfs -a
$ sudo systemctl restart nfs-kernel-server

# Allow access to all pis from NFS server
for host in pi0 pi1 pi2 pi3; do sudo ufw allow from `dig $host +short` to any port nfs; done
```



### Forward ports from outside world to ingress

This is needed on minikube since it builds a separate internal network that isn't exposed on the host. For pikube, no need to do this (like a real k8s cluster.)

```
ip -4 addr show scope global

# Try using -I instead of -A for the rules below

# NAT between internal IP and Ingress IP for locally generated packets (OUTPUT chain)
sudo iptables -t nat -I OUTPUT -p tcp -d 10.240.0.3 --dport 80 -j DNAT --to-destination 192.168.49.2:80

# NAT between external IP and Ingress IP for locally generated packets (OUTPUT chain)
sudo iptables -t nat -I OUTPUT -p tcp -d 34.74.108.195 --dport 80 -j DNAT --to-destination 192.168.49.2:80

# NAT between host IPs and Ingress IP for packets coming from the outside (PREROUTING)
sudo iptables -t nat -I PREROUTING -p tcp -d 34.74.108.195 --dport 80 -j DNAT --to-destination 192.168.49.2:80
sudo iptables -t nat -I PREROUTING -p tcp -d 10.240.0.3 --dport 80 -j DNAT --to-destination 192.168.49.2:80

# Allow IP forwarding from ens4 (host interface) to Ingress bridge interface
sudo iptables -A FORWARD -i ens4 -o br-f998b9bf1ee5 -p tcp --syn --dport 80 -m conntrack --ctstate NEW -j ACCEPT
sudo iptables -A FORWARD -i ens4 -o br-f998b9bf1ee5 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
sudo iptables -A FORWARD -i br-f998b9bf1ee5 -o ens4 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

```

### Debug iptables

```
sudo iptables -L
sudo iptables -L -t nat
sudo iptables -L -t nat --line

# delete rule 1 in chain OUTPUT
sudo iptables -D OUTPUT 1
```
