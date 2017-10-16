// netcat3 is a read/write TCP client.
package main

import (
	"io"
	"log"
	"net"
	"os"
)

func main() {
	conn, err := net.Dial("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}
	
    done := make(chan struct{})
    go func () {
        io.Copy(os.Stdout, conn) // server -> STDOUT; NOTE: ignoring errors
        log.Println("done")
        done <- struct{}{} // signal the main goroutine
    }()

    mustCopy(conn, os.Stdin) //  main go-routine: STDIN -> server        
    conn.Close()
    <-done // wait for background goroutine to finish
}

// mustCopy copies stuff from a reader to a writer or fails fatally.
func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
