package shuffle

// This file benchmarks three different approaches to writing a
// generic algorithm in Go.
//
// 0xFE - Mohit Cheppudira <mohit@muthanna.com>
//
// To run:
//    $ go test -benchmem -bench=. shuffle_test.go

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing"
	"time"
)

var (
	DATA100     = buildArray(100)
	DATA1000    = buildArray(1000)
	DATA10000   = buildArray(10000)
	DATA100000  = buildArray(100000)
	DATA1000000 = buildArray(1000000)
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

// Native shuffle. No generics. Shuffle a slice of ints.
func shuffle0(a []int) {
	for i := range a {
		j := rand.Intn(i + 1)
		a[i], a[j] = a[j], a[i]
	}
}

// Technique 1: requires user to convert specificially typed slice to
// a slice of interface{}. Shuffles the copy in-place.
func shuffle1(a []interface{}) {
	for i := range a {
		j := rand.Intn(i + 1)
		a[i], a[j] = a[j], a[i]
	}
}

// Technique 2: uses reflection to do the conversion then calls shuffle1. Returns
// a shuffled copy of the input.
func shuffle2(a interface{}) interface{} {
	val := reflect.ValueOf(a)
	out := make([]interface{}, val.Len())
	for i := 0; i < val.Len(); i++ {
		out[i] = val.Index(i).Interface()
	}

	shuffle1(out)
	return out
}

// Technique 3: Uses reflection entirely and does an in-place shuffle. Almost illegible.
func shuffle3(a interface{}) {
	val := reflect.ValueOf(a)
	tmp := reflect.New(val.Index(0).Type())

	for i := 0; i < val.Len(); i++ {
		j := rand.Intn(i + 1)
		tmp.Elem().Set(val.Index(j))
		val.Index(j).Set(val.Index(i))
		val.Index(i).Set(tmp.Elem())
	}
}

func buildArray(size int) []int {
	a := make([]int, size)
	for i := 0; i < size; i++ {
		a[i] = rand.Int()
	}

	return a
}

func bmShuffle0(data []int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		shuffle0(data)
	}
}

func bmShuffle1(data []int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		d := make([]interface{}, len(data))
		for i, x := range data {
			d[i] = interface{}(x)
		}

		shuffle1(d)
	}
}

func bmShuffle2(data []int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		shuffle2(data)
	}
}

func bmShuffle3(data []int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		shuffle3(data)
	}
}

// Shuffle 0
func BenchmarkShuffle0_100(b *testing.B) {
	bmShuffle0(DATA100, b)
}
func BenchmarkShuffle0_1000(b *testing.B) {
	bmShuffle0(DATA1000, b)
}
func BenchmarkShuffle0_10000(b *testing.B) {
	bmShuffle0(DATA10000, b)
}
func BenchmarkShuffle0_100000(b *testing.B) {
	bmShuffle0(DATA100000, b)
}
func BenchmarkShuffle0_1000000(b *testing.B) {
	bmShuffle0(DATA1000000, b)
}

// Shuffle 1
func BenchmarkShuffle1_100(b *testing.B) {
	bmShuffle1(DATA100, b)
}
func BenchmarkShuffle1_1000(b *testing.B) {
	bmShuffle1(DATA1000, b)
}
func BenchmarkShuffle1_10000(b *testing.B) {
	bmShuffle1(DATA10000, b)
}
func BenchmarkShuffle1_100000(b *testing.B) {
	bmShuffle1(DATA100000, b)
}
func BenchmarkShuffle1_1000000(b *testing.B) {
	bmShuffle1(DATA1000000, b)
}

// Shuffle 2
func BenchmarkShuffle2_100(b *testing.B) {
	bmShuffle2(DATA100, b)
}
func BenchmarkShuffle2_1000(b *testing.B) {
	bmShuffle2(DATA1000, b)
}
func BenchmarkShuffle2_10000(b *testing.B) {
	bmShuffle2(DATA10000, b)
}
func BenchmarkShuffle2_100000(b *testing.B) {
	bmShuffle2(DATA100000, b)
}
func BenchmarkShuffle2_1000000(b *testing.B) {
	bmShuffle2(DATA1000000, b)
}

// Shuffle 3
func BenchmarkShuffle3_100(b *testing.B) {
	bmShuffle3(DATA100, b)
}
func BenchmarkShuffle3_1000(b *testing.B) {
	bmShuffle3(DATA1000, b)
}
func BenchmarkShuffle3_10000(b *testing.B) {
	bmShuffle3(DATA10000, b)
}
func BenchmarkShuffle3_100000(b *testing.B) {
	bmShuffle3(DATA100000, b)
}
func BenchmarkShuffle3_1000000(b *testing.B) {
	bmShuffle3(DATA1000000, b)
}
