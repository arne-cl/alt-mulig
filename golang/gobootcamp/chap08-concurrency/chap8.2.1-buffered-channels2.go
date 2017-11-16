package main

import "fmt"

/*
This will work b/c the third write to the channel is made inside
a goroutine, which will wait until the channel is available.

$ go run chap8.2.1-buffered-channels2.go 
1
2
3
*/

func main() {
    c := make(chan int, 2)
    c <- 1
    c <- 2
    c3 := func() { c <- 3 }
    go c3()
    fmt.Println(<-c)
    fmt.Println(<-c)
    fmt.Println(<-c)
}
