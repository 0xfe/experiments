package main

import (
	"0xfe/experiments/minikube/dice"
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

var flagPort = flag.String("port", "3001", "GRPC server port")

type DiceServer struct {
	dice.UnimplementedRollServiceServer
	table map[string]*dice.RollTable
	id    int32

	mu *sync.Mutex
}

func NewDiceServer() *DiceServer {
	return &DiceServer{
		table: make(map[string]*dice.RollTable),
		mu:    &sync.Mutex{},
	}
}

func (s *DiceServer) Roll(ctx context.Context, req *dice.RollRequest) (*dice.RollResponse, error) {
	handle := req.RollerHandle

	defer s.mu.Unlock()
	s.mu.Lock()
	s.id++

	log.Printf("rolling %d for %s\n", s.id, handle)
	if t, ok := s.table[handle]; ok {
		t.Rolls = append(t.Rolls, &dice.DiceRoll{Id: s.id, Face: dice.Face(rand.Intn(2))})
	} else {
		t := &dice.RollTable{
			RollerHandle: handle,
			Rolls: []*dice.DiceRoll{
				{Id: s.id, Face: dice.Face_FACE_TAILS},
			},
		}

		s.table[handle] = t
	}

	response := &dice.RollResponse{}
	return response, nil
}

func (s *DiceServer) GetRolls(req *dice.GetRollsRequest, stream dice.RollService_GetRollsServer) error {
	defer s.mu.Unlock()
	s.mu.Lock()
	log.Printf("returning rolls...\n")
	for _, v := range s.table {
		err := stream.SendMsg(v)
		if err == io.EOF {
			return nil
		}

		if err != nil {
			return err
		}
	}

	return nil
}

func main() {
	flag.Parse()
	rand.Seed(time.Now().UnixNano())
	log.Printf("Starting GRPC RollServer on port %s...\n", *flagPort)

	lis, err := net.Listen("tcp", fmt.Sprintf(":%s", *flagPort))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	gsrv := grpc.NewServer()
	reflection.Register(gsrv) // for grpcurl
	diceServer := NewDiceServer()
	dice.RegisterRollServiceServer(gsrv, diceServer)
	if err := gsrv.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
