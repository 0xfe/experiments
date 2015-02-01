# Random Go Code

## Shuffle Test

`shuffle_test.go` tests various implementations of a generic Fischer-Yates
algorithm.

To run:
    $ go test -benchmem -bench=. shuffle_test.go

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

Shuffles `[]interface{}`. Requires user to convert `[]type` to `[]interface{}`.

```go
func shuffle1(a []interface{}) {
  for i := range a {
    j := rand.Intn(i + 1)
    a[i], a[j] = a[j], a[i]
  }
}
```

### 3. `shuffle2`

Shuffles `interface{}`. User does not need to convert slice, but data isn't shuffled in-place.

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

Shuffles `interface{}`. User does not need to convert slice, and the shuffle is performed in place. But since this uses reflection entirely, the implementation is grotesque.

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
BenchmarkShuffle0_100   300000        4585 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000     30000       45295 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_10000     3000      442840 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_100000       300     5854736 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle0_1000000       10   108399241 ns/op         0 B/op        0 allocs/op
BenchmarkShuffle1_100   200000        9964 ns/op      2464 B/op      101 allocs/op
BenchmarkShuffle1_1000     20000       98529 ns/op     24384 B/op     1001 allocs/op
BenchmarkShuffle1_10000     2000      993735 ns/op    243840 B/op    10001 allocs/op
BenchmarkShuffle1_100000       100    11417906 ns/op   2405632 B/op   100001 allocs/op
BenchmarkShuffle1_1000000       10   166154906 ns/op  24007174 B/op  1000001 allocs/op
BenchmarkShuffle2_100   100000       12762 ns/op      2528 B/op      103 allocs/op
BenchmarkShuffle2_1000     10000      123570 ns/op     24448 B/op     1003 allocs/op
BenchmarkShuffle2_10000     1000     1119474 ns/op    243904 B/op    10003 allocs/op
BenchmarkShuffle2_100000       100    12819098 ns/op   2405696 B/op   100003 allocs/op
BenchmarkShuffle2_1000000       10   188149365 ns/op  24007238 B/op  1000003 allocs/op
BenchmarkShuffle3_100   100000       18815 ns/op       832 B/op      101 allocs/op
BenchmarkShuffle3_1000     10000      200390 ns/op      8032 B/op     1001 allocs/op
BenchmarkShuffle3_10000     1000     1914459 ns/op     80032 B/op    10001 allocs/op
BenchmarkShuffle3_100000       100    24367265 ns/op    800032 B/op   100001 allocs/op
BenchmarkShuffle3_1000000        2   569379397 ns/op   8000040 B/op  1000001 allocs/op
ok    command-line-arguments  35.939s
```