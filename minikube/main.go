package main

import (
	"0xfe/experiments/minikube/dice"
	"fmt"
	"html"
	"log"
	"net/http"
)

const PORT = 3000

func main() {
	log.Printf("Starting webserver on port %d...\n", PORT)
	face := dice.Face(dice.Face_FACE_HEADS)

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, %q", html.EscapeString(r.URL.Path))
	})

	http.HandleFunc("/roll", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Roll: %d", face)
	})

	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", PORT), nil))
}
