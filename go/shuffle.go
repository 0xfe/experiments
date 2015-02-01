package main

// An implementation of a generic shuffle. Writing
// generic code in Go is sucky.

import (
	"fmt"
	"math/rand"
	"time"
)

func shuffle(a []interface{}) {
	for i := range a {
		j := rand.Intn(i + 1)
		a[i], a[j] = a[j], a[i]
	}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	a := []int64{1, 2, 3, 4, 5}

	b := make([]interface{}, len(a))
	for i, x := range a {
		b[i] = interface{}(x)
	}

	shuffle(b)
	fmt.Println(b)
}
