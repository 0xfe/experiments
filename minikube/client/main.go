package main

import (
	"0xfe/experiments/minikube/dice"
	"context"
	"fmt"
	"log"
	"math/rand"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Printf("hello world: %d\n", rand.Intn(2))

	var opts []grpc.DialOption
	opts = append(opts, grpc.WithTransportCredentials(insecure.NewCredentials()))
	conn, err := grpc.Dial("localhost:3001", opts...)

	if err != nil {
		log.Fatalf("could not connect to grpc server: %e", err)
	}

	defer conn.Close()
	client := dice.NewRollServiceClient(conn)

	resp, err := client.Roll(context.Background(), &dice.RollRequest{
		RollerHandle: "foobar",
	})

	if err != nil {
		log.Fatalf("error calling Roll: %e", err)
	}

	log.Printf("got response: %v\n", resp)
}
