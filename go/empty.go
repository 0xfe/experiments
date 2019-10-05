package main

import "fmt"

// Bottom is a valueless type
type Bottom struct{}

func main() {
	set := make(map[string]Bottom)
	exists := Bottom{}

	set["foo"] = exists
	set["bar"] = exists

	fmt.Printf("%v+\n", set)

	for k, v := range set {
		fmt.Printf("%v = %v\n", k, v)
	}
}
