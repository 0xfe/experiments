package main

// Framework for A* search. Reduces to Djikstra's algorithm.
// Mohit Cheppudira 2019

import (
	"container/heap"
	"fmt"
)

// Peer is a graph edge
type Peer struct {
	Cost int
	Node *Node
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
func (n *Node) AddPeer(node *Node, cost int) *Peer {
	peer := &Peer{cost, node}
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
	return h[i].Cost < h[j].Cost
}

func (h PeerHeap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *PeerHeap) Push(v interface{}) {
	*h = append(*h, v.(*Peer))
}

func (h *PeerHeap) Pop() interface{} {
	// Return the first element, and resize slice
	n := len(*h)
	ret := (*h)[n-1]
	*h = (*h)[:n-1]
	return ret
}

type PeerHeaps = map[*Node]*PeerHeap
type Path struct {
	Nodes []*Node
	Cost  int
}

func NewPath() *Path {
	return &Path{
		Nodes: []*Node{},
	}
}

func (p Path) Clone() *Path {
	newPath := &Path{[]*Node{}, p.Cost}
	for _, node := range p.Nodes {
		newPath.Nodes = append(newPath.Nodes, node)
	}

	return newPath
}

type AStarSearcher struct {
	From    *Node
	To      *Node
	paths   []*Path
	minPath *Path
	minCost int
}

func NewAStarSearcher(from *Node, to *Node) *AStarSearcher {
	return &AStarSearcher{
		From:    from,
		To:      to,
		paths:   []*Path{},
		minCost: -1,
	}
}

func (a *AStarSearcher) search(cur *Node, path *Path) (bool, *Path) {
	path.Nodes = append(path.Nodes, cur)

	if cur == a.To {
		// Found!
		a.paths = append(a.paths, path)
		return true, path
	}

	// If no paths from here, return false
	if len(cur.Peers) == 0 {
		return false, path
	}

	// Add peers to priority queue weighted by total cost
	peerHeaps := &PeerHeap{}
	heap.Init(peerHeaps)
	for _, peer := range cur.Peers {
		// Prune search if we already found a lower cost path
		if a.minCost == -1 || path.Cost+peer.Cost < a.minCost {
			peerHeaps.Push(&Peer{path.Cost + peer.Cost, peer.Node})
		}
	}

	// Search available paths and return the lowest cost path
	var minPath *Path
	for peerHeaps.Len() != 0 {
		peer := peerHeaps.Pop().(*Peer)
		newPath := path.Clone()
		newPath.Cost = peer.Cost
		found, foundPath := a.search(peer.Node, newPath)
		if found && (minPath == nil || foundPath.Cost < minPath.Cost) {
			minPath = foundPath
			a.minCost = minPath.Cost
		}
	}

	if minPath != nil {
		return true, minPath
	}

	return false, path
}

func (a *AStarSearcher) Search() (bool, *Path) {
	return a.search(a.From, NewPath())
}

func main() {
	// Create a graph with two paths to a target with different
	// costs.
	g := NewGraph("test")

	peer := g.Root.AddPeer(NewNode("foo"), 1)
	fmt.Printf("%+v\n", peer)

	peer = peer.Node.AddPeer(NewNode("bar"), 2)
	fmt.Printf("%+v\n", g.Root.Peers[0].Node.Peers[0].Node)

	peer1 := peer.Node.AddPeer(NewNode("bar1"), 1)
	peer2 := peer.Node.AddPeer(NewNode("bar2"), 2)

	peer1 = peer1.Node.AddPeer(NewNode("bars1"), 1)
	peer2 = peer2.Node.AddPeer(NewNode("bars2"), 2)

	peer1 = peer1.Node.AddPeer(NewNode("barsx1"), 1)
	peer2 = peer2.Node.AddPeer(NewNode("barsx2"), 2)

	target := NewNode("target")
	peer1.Node.AddPeer(target, 1)
	peer2.Node.AddPeer(target, 2)

	// Find the lowest cost path to target
	finder := NewAStarSearcher(g.Root, target)
	found, path := finder.Search()

	fmt.Println(found, path.Cost)
	for i, node := range path.Nodes {
		fmt.Println(i, node.Label)
	}
}
