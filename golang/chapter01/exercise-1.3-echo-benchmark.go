package main

import (
    "fmt"
    "os"
    "strings"
    "time"
)

func echo1() {
    var s, sep string
    for i := 1; i < len(os.Args); i++ {
        s += sep + os.Args[i]
        sep = " "
    }
    fmt.Println(s)
}

func echo2() {
    s, sep := "", ""
    for _, arg := range os.Args[1:] {
        s += sep + arg
        sep = " "
    }
    fmt.Println(s)
}

func echo3() {
    fmt.Println(strings.Join(os.Args[1:], " "))
}

func main() {
    start := time.Now()
    echo1()
    fmt.Printf("echo1: %.8fs elapsed\n", time.Since(start).Seconds())

    start = time.Now()
    echo2()
    fmt.Printf("echo2: %.8fs elapsed\n", time.Since(start).Seconds())

    start = time.Now()
    echo3()
    fmt.Printf("echo3: %.8fs elapsed\n", time.Since(start).Seconds())
}


// how to iterate over a list of function names?
//~ func main() {
    //~ for _, funcname := range (echo1 echo2 echo3) {
        //~ fmt.Println(funcname)
    //~ }
//~ }
