package main

import (
	"fmt"
	"html"
	"log"
	"net/http"
)

const PORT = 3000

func main() {
	log.Printf("Starting webserver on port %d...\n", PORT)
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, %q", html.EscapeString(r.URL.Path))
	})

	http.HandleFunc("/hi", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hi")
	})

	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", PORT), nil))
}
