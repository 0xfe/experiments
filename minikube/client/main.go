package main

import (
	"0xfe/experiments/minikube/dice"
	"context"
	"flag"
	"fmt"
	"io"
	"log"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var flagAddress = flag.String("target", "localhost:3001", "server address:port")

func main() {
	flag.Parse()
	log.Printf("connecting to %s...\n", *flagAddress)

	var opts []grpc.DialOption
	opts = append(opts, grpc.WithTransportCredentials(insecure.NewCredentials()))
	conn, err := grpc.Dial(*flagAddress, opts...)

	if err != nil {
		log.Fatalf("could not connect to grpc server: %+v", err)
	}

	defer conn.Close()
	client := dice.NewRollServiceClient(conn)
	roll := func(handle string) {
		_, err := client.Roll(context.Background(), &dice.RollRequest{
			RollerHandle: handle,
		})

		if err != nil {
			log.Fatalf("error calling Roll: %+v", err)
		}
	}

	roll("foobar")
	roll("foobar")
	roll("foobar")
	roll("0xfe")
	roll("0xfe")

	getClient, err := client.GetRolls(context.Background(), &dice.GetRollsRequest{})
	if err != nil {
		log.Fatalf("error calling GetRolls: %+v", err)
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
			log.Fatalf("failed receiving message: %+v", err)
		}

		fmt.Printf("handle: %v\n", msg.RollerHandle)
		for i, roll := range msg.Rolls {
			fmt.Printf("  roll %d: %v (%d)\n", i+1, roll.Face, roll.Id)
		}
	}
}
