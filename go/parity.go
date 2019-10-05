package main

import "fmt"

// Parity is a naive parity calculator
func Parity(v int64) int64 {
	parity := int64(0)

	for v > 0 {
		parity ^= v & 1
		v >>= 1
	}

	return parity
}

func main() {
	fmt.Printf("1 << 0: %v\n", 1<<0)
	fmt.Printf("1 << 1: %v\n", 1<<1)
	fmt.Printf("1 << 2: %v\n", 1<<2)
	fmt.Printf("1 << 3: %v\n", 1<<3)
	fmt.Printf("parity of 1: %v\n", Parity(1))
	fmt.Printf("parity of 2: %v\n", Parity(2))
	fmt.Printf("parity of 3: %v\n", Parity(3))
	fmt.Printf("parity of 4: %v\n", Parity(4))
}
