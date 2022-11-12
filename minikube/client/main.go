package main

import (
	"0xfe/experiments/minikube/dice"
	"context"
	"fmt"
	"io"
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
	roll := func(handle string) {
		_, err := client.Roll(context.Background(), &dice.RollRequest{
			RollerHandle: handle,
		})

		if err != nil {
			log.Fatalf("error calling Roll: %e", err)
		}
	}

	roll("foobar")
	roll("foobar")
	roll("foobar")
	roll("0xfe")
	roll("0xfe")

	getClient, err := client.GetRolls(context.Background(), &dice.GetRollsRequest{})
	if err != nil {
		log.Fatalf("error calling GetRolls: %e", err)
	}

	for {
		var msg dice.RollTable
		err := getClient.RecvMsg(&msg)
		if err == io.EOF {
			getClient.CloseSend()
			log.Println("all done")
			return
		}

		if err != nil {
			log.Fatalf("failed receiving message: %e", err)
		}

		fmt.Printf("handle: %v\n", msg.RollerHandle)
		fmt.Printf("  rolls: %v\n", msg.Rolls)
	}
}
