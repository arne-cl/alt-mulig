package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "bufio"
)

func main() {
    if len(os.Args) == 1 {
        input := bufio.NewScanner(os.Stdin)
        for input.Scan() {
            fmt.Println(input.Text())
        }
    } else {
        for _, filename := range os.Args[1:] {
            data, err := ioutil.ReadFile(filename)
            if err != nil {
                fmt.Fprintf(os.Stderr, "cat: %v\n", err)
            }
            fmt.Printf("%s", string(data))
        }
    }
}
