package main

import (
	"fmt"
	"time"
)

// spinner prints an ASCII-art spinning wheel with the given pause/dalay
// between each step.
func spinner(delay time.Duration) {
	for {
		for _, symbol := range `–\|/~` {
			fmt.Printf("\r%c", symbol)
			time.Sleep(delay)
		}
	}
}

// fib calculates the nth fibonacci number recursively (slow by design).
func fib(x int) int {
	if x < 2 {
		return x
	} else {
		return fib(x-1) + fib(x-2)
	}
}


func main() {
	go spinner(100 * time.Millisecond)
	fmt.Printf("\rFibonacci(45) = %d\n", fib(45))
}
