// netcat2 is a read/write TCP client.
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
	defer conn.Close()

	go mustCopy(os.Stdout, conn) // read from server, write to STDOUT
	mustCopy(conn, os.Stdin) //  main go-routine: STDIN -> server
}

// mustCopy copies stuff from a reader to a writer or fails fatally.
func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
