package main

import (
    "fmt"
    "os"
)

func main() {
    for i, arg := range os.Args {
        fmt.Println(i, arg)
     //   fmt.Println(strings.Join(os.Args, " "))
    }
}
