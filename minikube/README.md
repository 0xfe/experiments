# HOWTO

## Setup

```
minikube start
eval $(minikube -p minikube docker-env)
```

## Run

```
# main
go run main.go

# server
cd server
go run main.go

# curl
$ grpcurl -plaintext localhost:3001 describe
RollService is a service:
service RollService {
  rpc GetRolls ( .GetRollsRequest ) returns ( stream .GetRollsResponse );
  rpc Roll ( .RollRequest ) returns ( .RollResponse );
}
grpc.reflection.v1alpha.ServerReflection is a service:
service ServerReflection {
  rpc ServerReflectionInfo ( stream .grpc.reflection.v1alpha.ServerReflectionRequest ) returns ( stream .grpc.reflection.v1alpha.ServerReflectionResponse );
}

grpcurl -plaintext -d '{"roller_handle": "0xfe"}' localhost:3001 RollService/Roll

# client
cd client
go run main.go --target 3001
```

## Run in k8s / minikube

Make sure deployment has "imagePullPolicy: Never".

```
kubectl apply -f k8s/main-depl.yaml
kubectl apply -f k8s/server-depl.yaml
kubectl apply -f k8s/ingress.yaml

kubectl get pods
kubectl get ingress

$ curl 192.168.49.2
```

## Build

### Build protos

```
protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative dice.proto

```

### Build container image

```
# If pushing to minikube, point docker to local minikube image repo
eval $(minikube -p minikube docker-env)

# build protos
cd dice
protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative dice.proto

cd ..

# builds and pushes to local repo
docker build . -f server.Dockerfile -t 0xfe/dice/server
docker build . -f Dockerfile -t 0xfe/dice/main
```

### Run container on host

Run these in a separate shell that's connected to the local docker daemon, not the minkube one.

```
docker run -p 3000:3000 0xfe/main
docker run -p 3001:3001 0xfe/dice/server

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