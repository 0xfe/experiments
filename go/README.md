## A Generic Shuffle in Go

Go does not have generics, but it does have reflection. It also has a kind of catch-all dynamic type: `interface{}`. You can put these together to write generic algorithms and containers.

This is a short analysis fo the performance implications of writing a generic alogrithm in Go.

Note that although you can use these techniques to reduce duplication in your code, you compromise on compile-time type saftey, which increases the liklihood of run-time bugs.

## The Code

The file `shuffle_test.go` in this directory benchmarks various implementations of a generic Fischer-Yates algorithm.

To run:

    $ go test -benchmem -bench=. shuffle_test.go -benchtime=10s

There are 4 types of `shuffle`:

### 1. `shuffle0`

Non generic version. Shuffles `[]int` in-place.

```go
func shuffle0(a []int) {
  for i := range a {
    j := rand.Intn(i + 1)
    a[i], a[j] = a[j], a[i]
  }
}
```

### 2. `shuffle1`

Shuffles `[]interface{}`. Requires user to convert `[]type` to `[]interface{}` before calling. Data is shuffled in-place.

```go
func shuffle1(a []interface{}) {
  for i := range a {
    j := rand.Intn(i + 1)
    a[i], a[j] = a[j], a[i]
  }
}
```

### 3. `shuffle2`

Shuffles `interface{}`. Uses reflection to determine the type and convert the slice, so the user does not need to preprocess before calling. Data is not shuffled in-place. The actual shuffling is performed by `shuffle1`.

```go
func shuffle2(a interface{}) interface{} {
  val := reflect.ValueOf(a)
  out := make([]interface{}, val.Len())
  for i := 0; i < val.Len(); i++ {
    out[i] = val.Index(i).Interface()
  }

  shuffle1(out)
  return out
}
```

### 4. `shuffle3`

Shuffles `interface{}`. User does not need to convert slice, and the shuffle is performed in-place. But since this uses reflection entirely, the implementation is grotesque.

```go
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
```

## The Results

As of 2015/02/01 on my 1.7Ghz Core i7 MacBook Air with 8G 1600 MHz DDR3 RAM running OS X 10.9.5:

The tests were performed with 5 datasets, each with 100, 1000, 10^4, 10^5, and 10^6 elements. Test name `BenchmarkShuffle1_10000` refers to a benchmark of `shuffle1` on a 10000 element slice.

Fields: test name, iterations, time per iteration, bytes allocated per iteration, number of allocations per iteration

```
$ go test -benchmem -bench=. shuffle_test.go -benchtime=10s
BenchmarkShuffle0_100  3000000        4231 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000    300000       41704 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_10000    30000      416090 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_100000      3000     4554965 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000000      200    83843609 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle1_100  2000000        8795 ns/op      2464 B/op      101 allocs/op
BenchmarkShuffle1_1000    200000       85891 ns/op     24384 B/op     1001 allocs/op
BenchmarkShuffle1_10000    20000      868768 ns/op    243840 B/op    10001 allocs/op
BenchmarkShuffle1_100000      2000     9321111 ns/op   2405632 B/op   100001 allocs/op
BenchmarkShuffle1_1000000      100   150531242 ns/op  24007173 B/op  1000001 allocs/op
BenchmarkShuffle2_100  1000000       11198 ns/op      2528 B/op      103 allocs/op
BenchmarkShuffle2_1000    200000      106654 ns/op     24448 B/op     1003 allocs/op
BenchmarkShuffle2_10000    10000     1090785 ns/op    243904 B/op    10003 allocs/op
BenchmarkShuffle2_100000      2000    11422546 ns/op   2405696 B/op   100003 allocs/op
BenchmarkShuffle2_1000000      100   173314206 ns/op  24007237 B/op  1000003 allocs/op
BenchmarkShuffle3_100  1000000       13421 ns/op        40 B/op        2 allocs/op
BenchmarkShuffle3_1000    100000      139189 ns/op        40 B/op        2 allocs/op
BenchmarkShuffle3_10000    10000     1460109 ns/op        40 B/op        2 allocs/op
BenchmarkShuffle3_100000      1000    13464979 ns/op        40 B/op        2 allocs/op
BenchmarkShuffle3_1000000      100   183529639 ns/op        40 B/op        2 allocs/op
ok    command-line-arguments  355.166s
```

## Conclusion

* `shuffle0` is the baseline. It represents the cost of a native (non-generic) implementation, or ad-hoc generics using some kind of source rewriting, e.g., with `go generate`. It is type safe.

* `shuffle1` costs about 1.8x `shuffle0` for large datasets, and about 2x for small sets. It uses O(N) additional space (allocated prior to calling). It is an unfriendly implementation, since the user is required to convert a static type (`[]int`) into a dynamic (for lack of a better word) one (`[]interface{}`) before passing it in.

* `shuffle2` costs about 2x `shuffle0` for large sets, and about 2.5x for small sets. It also uses O(N) additional space. The implementation is effectively the same `shuffle1`, except that it relieves the user from having to preprocess the slice. For a single call, `shuffle2` costs the same as `shuffle1`.

* `shuffle3` costs about 2.2x `shuffle0` for large sets, and about 3x for small sets. It uses no additional space (except for a single extra temporary variable). This is the slowest technique, and also the ugliest. But the tradeoff here is the lack of memory overhead.

-- Mohit Cheppudira - 0xFE