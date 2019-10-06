package main

import "fmt"

// Peer is a graph edge
type Peer struct {
	Weight int
	Node   *Node
}

// Node is a vertex in a graph
type Node struct {
	Label string
	Peers []*Peer // List of weighted peers
}

// NewNode creates a new graph node
func NewNode(label string) *Node {
	return &Node{
		Label: label,
		Peers: []*Peer{},
	}
}

// AddPeer adds node as a peer
func (n *Node) AddPeer(node *Node, weight int) *Peer {
	peer := &Peer{weight, node}
	n.Peers = append(n.Peers, peer)
	return peer
}

// StateMap is a list of global k/v pairs
type StateMap map[string]interface{}

// Graph is a convenience struct for global graph state
type Graph struct {
	Root  *Node
	State StateMap
}

// NewGraph creates a new graph
func NewGraph(label string) *Graph {
	return &Graph{
		Root:  NewNode(label),
		State: StateMap{},
	}
}

// PeerHeap is a priority queue of peers
type PeerHeap []*Peer

func (h PeerHeap) Len() int {
	return len(h)
}

func (h PeerHeap) Less(i, j int) bool {
	return h[i].Weight < h[j].Weight
}

func (h PeerHeap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func main() {
	g := NewGraph("test")

	peer := g.Root.AddPeer(NewNode("foo"), 1)
	fmt.Printf("%+v\n", peer)

	peer = peer.Node.AddPeer(NewNode("bar"), 2)
	fmt.Printf("%+v\n", g.Root.Peers[0].Node.Peers[0].Node)
}
