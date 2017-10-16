// reverb1 will is a TCP server that echos the input 3x,
// e.g. Hallo -> HALLO, Hallo, hallo.
//
// NOTE: This implementation will not start to echo a new
// input until the old input is echoed 3x.
package main

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"strings"
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
		go handleConnection(conn) // handle one connection at a time
	}
}

// handleConnection echos the input three times (HELLO, Hello, hello).
func handleConnection(c net.Conn) {
	input := bufio.NewScanner(c)
	for input.Scan() {
		go echo(c, input.Text(), 1 * time.Second)
	} // NOTE: we ignore errors from input.Err()
}

func echo(c net.Conn, shout string, delay time.Duration) {
	fmt.Fprintln(c, "\t", strings.ToUpper(shout))
	time.Sleep(delay)
	fmt.Fprintln(c, "\t", shout)
	time.Sleep(delay)
	fmt.Fprintln(c, "\t", strings.ToLower(shout))
	time.Sleep(delay)

}
