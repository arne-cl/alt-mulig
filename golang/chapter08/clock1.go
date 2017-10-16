// clock1 creates a TCP listener on port 8000. If you connect to it,
// the service will print the time to the socket once per second.
//
// You can test this by running 'nc localhost 8000'.
// Note: this implementation is blocking, i.e. if a second connection
// comes in, it will see any "time messages" before the first
// connection is closed.
package main

import (
	"io"
	"log"
	"net"
	"time"
)

func main() {
	listener, err := net.Listen("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil { // e.g. if connection was aborted
			log.Print(err)
			continue
		}
		handleConnection(conn) // handle one connection at a time
	}
}

// handleConnection wtites the time to an incoming connection once a second.
func handleConnection(c net.Conn) {
	defer c.Close()

	for {
		_, err := io.WriteString(c, time.Now().Format("15:04:05\n"))
		if err != nil {
			return // e.g. if connection was aborted
		}
		time.Sleep(1 * time.Second)
	}
}
