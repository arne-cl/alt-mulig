package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "strings"
)


//~ func countLinesInString(s string, counts map[string]int) {
    //~ for _, line := range strings.Split(s, "\n") {
        //~ counts[line]++
    //~ }
//~ }
//~ 
//~ func printCounts(counts map[string]int) {
    //~ for line, n := range counts {
        //~ if n > 1 {
            //~ fmt.Printf("%d\t%s\n", n, line)
        //~ }
    //~ }
//~ }


func main() {
    counts := make(map[string]int)
    //~ fmt.Println("Args[1:]", os.Args[1:])
    for _, filename := range os.Args[1:] {
        // TODO: how to get the type of a var?
        data, err := ioutil.ReadFile(filename)
        if err != nil {
            fmt.Fprintf(os.Stderr, "dup3: %v\n", err)
            continue            
        }
        for _, line := range strings.Split(string(data), "\n") {
            counts[line]++
        }
    }
    for line, n := range counts {
        if n > 1 {
            fmt.Printf("%d\t%s\n", n, line)
        }
    }

}

