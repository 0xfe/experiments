= Random Go Code

== Shuffle Test

`shuffle_test.go` tests various implementations of a generic Fischer-Yates
algorithm.

To run:
    $ go test -benchmem -bench=. shuffle_test.go