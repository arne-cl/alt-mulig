package main

/*

Using a goroutine, we can't be certain which function is ready first.

$ go run chap8.1-goroutines1.go 
world
hello
hello
world
world
hello
hello
world
world
hello

*/

import (
	"fmt"
	"time"
)

func say(s string) {
	for i := 0; i < 5; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}

func helloworldConcurrent() {
	go say("hello")
	say("world")
}

func helloworldIterative() {
	say("hello")
	say("world")
}



func main() {
	fmt.Println("Iterative hello-world")
	helloworldIterative()
	fmt.Println("\nConcurrent hello-world")
	helloworldConcurrent()
}

