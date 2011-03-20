package main

import ("os"
        "flag")

var omitNewLine = flag.Bool("n", false, "Omit new line")

const (Space = " "; Newline = "\n")

func main() {
  flag.Parse()

  var s string = "";

  for i := 0; i < flag.NArg(); i++ {
    if i > 0 { s += Space }

    s += flag.Arg(i);
  }

  if !*omitNewLine { s += Newline }

  os.Stdout.WriteString(s)
}
