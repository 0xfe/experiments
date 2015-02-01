package main

// An implementation of a generic shuffle. Writing
// generic code in Go is sucky.

import (
	"fmt"
	"math/rand"
	"reflect"
	"time"
)

func shuffle1(a []interface{}) {
	for i := range a {
		j := rand.Intn(i + 1)
		a[i], a[j] = a[j], a[i]
	}
}

func shuffle2(a interface{}) interface{} {
	val := reflect.ValueOf(a)
	out := make([]interface{}, val.Len())
	for i := 0; i < val.Len(); i++ {
		out[i] = val.Index(i).Interface()
	}

	shuffle1(out)
	return out
}

func shuffle3(a interface{}) {
	val := reflect.ValueOf(a)

	for i := 0; i < val.Len(); i++ {
		j := rand.Intn(i + 1)
		tmp := reflect.ValueOf(val.Index(j).Interface())
		val.Index(j).Set(val.Index(i))
		val.Index(i).Set(tmp)
	}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	// Technique 1
	a := []int64{1, 2, 3, 4, 5}

	b := make([]interface{}, len(a))
	for i, x := range a {
		b[i] = interface{}(x)
	}

	shuffle1(b)
	fmt.Println(b)

	// Technique 2
	c := shuffle2(a)
	fmt.Println(c)

	// Technique 3
	shuffle3(a)
	fmt.Println(a)
}
