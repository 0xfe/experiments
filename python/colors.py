#!/usr/bin/env python

class Colors:
  ESC = "\033["
  PURPLE = ESC + "95m"
  GREEN = ESC + "92m"
  YELLOW = ESC + "93m"
  RESET = ESC + "0m"

print Colors.YELLOW + "Warning: Colors enabled."
print Colors.RESET
