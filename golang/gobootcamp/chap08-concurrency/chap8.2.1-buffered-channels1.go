package main

/*
Sends to a buffered channel block only when the buffer is full. Receives block when the buffer is empty.

$ go run chap8.2.1-buffered-channels1.go 
use channel up to its capacity:
1
2
use channel beyond its capacity:
fatal error: all goroutines are asleep - deadlock!

goroutine 1 [chan send]:
main.main()
        /home/arne/repos/alt-mulig/golang/gobootcamp/chap08-concurrency/chap8.2.1-buffered-channels1.go:20 +0x3b6
exit status 2

*/
import "fmt"

func main() {
    fmt.Println("use channel up to its capacity:")
    c := make(chan int, 2)
    c <- 1
    c <- 2
    fmt.Println(<-c)
    fmt.Println(<-c)

    fmt.Println("use channel beyond its capacity:")
    d := make(chan int, 2)
    d <- 1
    d <- 2
    d <- 3
    fmt.Println(<-c)
    fmt.Println(<-c)
    fmt.Println(<-c)
}
