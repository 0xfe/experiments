package main

import ("fmt")

func sum(arr []int) int {
  total := 0

  for i := 0; i < len(arr); i++ {
    total += arr[i]
  }

  return total
}

func main() {
  total := sum([]int{105, 4, 3, 2, 1})
  fmt.Printf("Total = %d\n", total);
}
