package main

import "fmt"

func main() {
    i := 10
    // works like a traditional while-loop
    for i > 0 {
        fmt.Println(i)
        i--
    }
}
