package main

/*
This tries to "speed up" summing over an array of ints by splitting it
into two slices, calling the sum function over both parts independently
(using goroutines) and then adding the two sums back together.

By default, sends and receives block wait until the other side is ready.
This allows goroutines to synchronize without explicit locks or
condition variables.

$ go run chap8.2-channels1.go 
-5 17 12
*/

import "fmt"

func sum(a []int, c chan int) {
	sum := 0
	for _, v := range a {
		sum += v
	}
	c <- sum // send sum to c
}

func main() {
	a := []int{7, 2, 8, -9, 4, 0}

	c := make(chan int)
	go sum(a[:len(a)/2], c)
	go sum(a[len(a)/2:], c)
	x, y := <-c, <-c // receive from c

	fmt.Println(x, y, x+y)
}

