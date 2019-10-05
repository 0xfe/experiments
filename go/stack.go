package main

import "fmt"

// Stack is an implementation of a stack data structure. This implementation
// is optimized for deterministic latency, at the expense of additional memory.
//
// The GC() function can be called to reclaim mamory when it is safe to do so.
type Stack struct {
	elements []interface{}
	top      int
}

// Push adds an element to the stack
func (s *Stack) Push(v interface{}) {
	if len(s.elements) > s.top {
		s.elements[s.top] = v
	} else {
		s.elements = append(s.elements, v)
	}
	s.top++
}

// Pop removes an element from the stack and returns it
func (s *Stack) Pop() (interface{}, error) {
	if s.top > 0 {
		s.top--
		return s.elements[s.top], nil
	}

	return nil, fmt.Errorf("empty stack")
}

// GC garbage collects popped elements
func (s *Stack) GC() {
	if s.top > 0 && len(s.elements) > s.top {
		s.elements = s.elements[:s.top]
	}
}

// Size returns the size of the stack
func (s Stack) Size() int {
	return s.top
}

func (s Stack) String() string {
	v := ""
	for e := 0; e < s.top; e++ {
		v += fmt.Sprintf("%v ", s.elements[e])
	}

	return fmt.Sprintf("%s%v %v", v, s.Size(), len(s.elements))
}

func main() {
	s := &Stack{}

	s.Push("bob")
	s.Push("boo")
	s.Push("foo")

	v, err := s.Pop()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Popped: %v\n", v)

	s.Pop()
	v, err = s.Pop()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Popped: %v\n", v)
	s.Push("bar")

	fmt.Printf("%v\n", s)
	s.GC()
	fmt.Printf("%v\n", s)
}
