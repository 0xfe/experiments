package main

import (
	"0xfe/experiments/minikube/client"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
)

var flagPort = flag.Int("port", 3000, "server address:port")
var flagTarget = flag.String("target", "localhost:3001", "server address:port")

func main() {
	flag.Parse()
	target := os.Getenv("DICE_GRPC_TARGET")
	if target == "" {
		target = *flagTarget
	}

	log.Printf("connecting to dice gRPC server at %s...\n", target)

	client := client.NewClient(target)
	defer client.Close()

	http.HandleFunc("/getrolls", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		ch, err := client.GetRolls()
		if err != nil {
			log.Fatalf("failed: %+v", err)
		}

		for rt := range ch {
			fmt.Fprintf(w, "handle: %v\n", rt.RollerHandle)
			for i, roll := range rt.Rolls {
				fmt.Fprintf(w, "  roll %d: %v (%d)\n", i+1, roll.Face, roll.Id)
			}
		}
	})

	http.HandleFunc("/roll", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		client.Roll("foobar")
		fmt.Fprintf(w, "Rolled\n")
	})

	log.Printf("starting webserver on port %d...\n", *flagPort)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", *flagPort), nil))
}
