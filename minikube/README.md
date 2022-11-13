# Minikube Experiment

This repo implements an end-to-end web and commandline "diceroll app" with a gRPC backend running on k8s. Uses:

- Go 1.19
- gRPC / Protobuf
- Docker
- Minikube
- nginx Ingress
- iptables rules for exposing minikube bridge network to the outside world

Components:

- `server/` - GRPC server binary. Built with `server.Dockerfile`.
- `client/` - GRPC client library.
  - `client/cmd` - Commandline tool that uses the client library to talk to the server
- `main.go` - Web server that talks to the gRPC server. Exposes `/roll` and `/getrolls`

## Quick Run

Make sure you have the gRPC/protobuf dependencies. See the "build" section below.

```
# Build protobuf stubs (once only, or when you change the proto files)
$ protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative dice.proto

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

## Run in k8s / minikube

### Start Minikube

```
minikube start
eval $(minikube -p minikube docker-env)
```

### Create deployments and ingress

Make sure deployment has "imagePullPolicy: Never".

```
kubectl apply -f k8s/main-depl.yaml
kubectl apply -f k8s/server-depl.yaml
kubectl apply -f k8s/ingress.yaml

kubectl get pods

# get ingress IP address
kubectl get ingress

$ curl 192.168.49.2
```

## Debug

```
# Run shell in new container (first time)
$ k run temppod --image=radial/busyboxplus:curl -it

# Same thing, but with debian instead
$ k run temppod --image=debian -it
temppod# cat /etc/debian_version
temppod# apt update
temppod# apt install curl
temppod# curl main-sevice:3000

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
```

## Build

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

#
### Forward ports from outside world to ingress

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

# OLD STUFF

```

###sudo iptables -t nat -A POSTROUTING -j MASQUERADE

??
sudo iptables -t nat -I PREROUTING -p tcp -i ens4 --dport 80 -j DNAT --to 192.168.49.2
sudo iptables -t nat -I POSTROUTING -p tcp -o ens4 --dport 80 -d 192.168.49.2 -j SNAT --to 10.240.0.3


### more specific, but does not work
sudo iptables -t nat -A PREROUTING -p tcp -d 10.240.0.3 --dport 80 -j DNAT --to-destination 192.168.49.2:80
sudo iptables -t nat -A PREROUTING -p tcp -d 34.74.108.195 --dport 80 -jDNAT --to-destination 192.168.49.2:80

sudo iptables -A FORWARD -i ens4 -o br-f998b9bf1ee5 -p tcp --syn --dport 80 -m conntrack --ctstate NEW -j ACCEPT
sudo iptables -A FORWARD -i ens4 -o br-f998b9bf1ee5 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
sudo iptables -A FORWARD -i br-f998b9bf1ee5 -o ens4 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

# if filtered
sudo iptables -I INPUT -p tcp --dport 80 -j ACCEPT
sudo iptables -I OUTPUT -p tcp --sport 80 -j ACCEPT
sudo iptables -I FORWARD -p tcp -d 192.168.49.2 --dport 80 -j ACCEPT
sudo iptables -I FORWARD -p tcp -d 10.240.0.3 --dport 80 -j ACCEPT

iptables -t nat -A POSTROUTING -o ens4 -p tcp -d 192.168.49.2 --dport 80 -j RETURN

sudo tcpdump -i  ens4 port 80

iptables -t nat -A PREROUTING -p tcp -m tcp -i ens0 --dport 80 -j REDIRECT --to-ports 8080


```