package main

import (
	"0xfe/experiments/minikube/client"
	"flag"
	"fmt"
	"log"
)

var flagTarget = flag.String("target", "localhost:3001", "server address:port")

func main() {
	flag.Parse()
	log.Printf("connecting to %s...\n", *flagTarget)

	client := client.NewClient(*flagTarget)
	defer client.Close()

	client.Roll("foobar")
	client.Roll("foobar")
	client.Roll("foobar")
	client.Roll("0xfe")
	client.Roll("0xfe")

	ch, err := client.GetRolls()
	if err != nil {
		log.Fatalf("failed: %+v", err)
	}

	for rt := range ch {
		fmt.Printf("handle: %v\n", rt.RollerHandle)
		for i, roll := range rt.Rolls {
			fmt.Printf("  roll %d: %v (%d)\n", i+1, roll.Face, roll.Id)
		}
	}
}
