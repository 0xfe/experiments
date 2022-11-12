package main

import (
	"0xfe/experiments/minikube/dice"
	"context"
	"fmt"
	"log"
	"net"
	"sync"

	"google.golang.org/grpc"
)

const PORT = 3001

type DiceServer struct {
	dice.UnimplementedRollServiceServer
	table map[string]*dice.RollTable

	mu *sync.Mutex
}

func NewDiceServer() *DiceServer {
	return &DiceServer{
		table: map[string]*dice.RollTable{},
	}
}

func (s *DiceServer) Roll(ctx context.Context, req *dice.RollRequest) (*dice.RollResponse, error) {
	handle := req.RollerHandle

	defer s.mu.Unlock()
	s.mu.Lock()
	if t, ok := s.table[handle]; ok {
		t.Rolls = append(t.Rolls, &dice.DiceRoll{Id: 1, Face: dice.Face_FACE_HEADS})
	} else {
		t := &dice.RollTable{
			RollerHandle: handle,
			Rolls: []*dice.DiceRoll{
				{Id: 0, Face: dice.Face_FACE_TAILS},
			},
		}

		s.table[handle] = t
	}

	response := &dice.RollResponse{}
	return response, nil
}

func main() {
	log.Printf("Starting GRPC RollServer on port %d...\n", PORT)

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", PORT))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	gsrv := grpc.NewServer()
	diceServer := NewDiceServer()
	dice.RegisterRollServiceServer(gsrv, diceServer)
	if err := gsrv.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
