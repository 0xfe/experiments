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

type Client struct {
	target string
	client dice.RollServiceClient
	conn   *grpc.ClientConn
}

func NewClient(target string) *Client {
	var opts []grpc.DialOption
	opts = append(opts, grpc.WithTransportCredentials(insecure.NewCredentials()))
	conn, err := grpc.Dial(target, opts...)

	if err != nil {
		log.Fatalf("could not connect to grpc server: %+v", err)
	}

	return &Client{target, dice.NewRollServiceClient(conn), conn}
}

func (c *Client) Close() {
	c.conn.Close()
}

func (c *Client) Roll(handle string) {
	_, err := c.client.Roll(context.Background(), &dice.RollRequest{
		RollerHandle: handle,
	})

	if err != nil {
		log.Fatalf("error calling Roll: %+v", err)
	}
}

func (c *Client) GetRolls() (chan *dice.RollTable, error) {
	getClient, err := c.client.GetRolls(context.Background(), &dice.GetRollsRequest{})
	if err != nil {
		return nil, fmt.Errorf("error calling GetRolls: %w", err)
	}

	ch := make(chan *dice.RollTable)

	go func() {
		for {
			var msg dice.RollTable
			err := getClient.RecvMsg(&msg)
			if err == io.EOF {
				getClient.CloseSend()
				close(ch)
				return
			}

			if err != nil {
				log.Fatalf("failed receiving message: %+v", err)
			}

			ch <- &msg
		}
	}()

	return ch, nil
}

func main() {
	flag.Parse()
	log.Printf("connecting to %s...\n", *flagAddress)

	client := NewClient(*flagAddress)
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
