# Random Go Code

## Shuffle Test

`shuffle_test.go` tests various implementations of a generic Fischer-Yates
algorithm.

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

Shuffles `interface{}`. Uses reflection to determine the type and convert the slice, so the user does not need to preprocess before calling. Data is not shuffled in-place.

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

  for i := 0; i < val.Len(); i++ {
    j := rand.Intn(i + 1)
    tmp := reflect.ValueOf(val.Index(j).Interface())
    val.Index(j).Set(val.Index(i))
    val.Index(i).Set(tmp)
  }
}
```

## The Results

As of 2015/02/01 on my 1.7Ghz Core i7 MacBook Air with 8G 1600 MHz DDR3 RAM running OS X 10.9.5:

```
$ go test -benchmem -bench=. shuffle_test.go -benchtime=10s
testing: warning: no tests to run
PASS
BenchmarkShuffle0_100  3000000        4361 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000    300000       43106 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_10000    30000      426872 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_100000      3000     4525749 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000000      200    81244085 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle1_100  2000000        8747 ns/op      2464 B/op      101 allocs/op
BenchmarkShuffle1_1000    200000       86204 ns/op     24384 B/op     1001 allocs/op
BenchmarkShuffle1_10000    20000      882846 ns/op    243840 B/op    10001 allocs/op
BenchmarkShuffle1_100000      2000     9291741 ns/op   2405632 B/op   100001 allocs/op
BenchmarkShuffle1_1000000      100   153209564 ns/op  24007173 B/op  1000001 allocs/op
BenchmarkShuffle2_100  1000000       10913 ns/op      2528 B/op      103 allocs/op
BenchmarkShuffle2_1000    200000      113354 ns/op     24448 B/op     1003 allocs/op
BenchmarkShuffle2_10000    10000     1220831 ns/op    243904 B/op    10003 allocs/op
BenchmarkShuffle2_100000      2000    12294690 ns/op   2405696 B/op   100003 allocs/op
BenchmarkShuffle2_1000000      100   195267815 ns/op  24007237 B/op  1000003 allocs/op
BenchmarkShuffle3_100  1000000       18362 ns/op       832 B/op      101 allocs/op
BenchmarkShuffle3_1000    100000      181565 ns/op      8032 B/op     1001 allocs/op
BenchmarkShuffle3_10000    10000     1796956 ns/op     80032 B/op    10001 allocs/op
BenchmarkShuffle3_100000      1000    19094388 ns/op    800032 B/op   100001 allocs/op
BenchmarkShuffle3_1000000       50   242966292 ns/op   8000039 B/op  1000001 allocs/op
ok    command-line-arguments  374.509s
```

## Conclusions

* `shuffle1` costs about 2x `shuffle0`
* `shuffle2` costs about 2.2x `shuffle0`
* `shuffle3` costs about 3x `shuffle0`