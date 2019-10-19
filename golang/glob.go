package main

import (
    "fmt"
    "os"
    "path/filepath"
)

func main() {
    path := os.Args[1]
    matches, err := filepath.Glob(path)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Can't parse glob path '%s': %v\n", path, err)
        return
    }
    fmt.Printf("%s\n", path)
    for _, match := range matches {
        fmt.Printf("\t%v\n", match)
    }
}
