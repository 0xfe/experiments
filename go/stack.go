package main

import "fmt"

// Stack is an implementation of a stack data structure
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

// Size returns the size of the stack
func (s Stack) Size() int {
	return s.top
}

func (s Stack) String() string {
	v := ""
	for e := 0; e < s.top; e++ {
		v += fmt.Sprintf("%v ", s.elements[e])
	}

	return v
}

func main() {
	s := &Stack{}

	s.Push("bob")
	s.Push("boo")
	s.Push("foo")
	s.Pop()
	s.Pop()
	s.Push("bar")

	fmt.Printf("%v\n", s)
}
