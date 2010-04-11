#!/usr/bin/env python

import curses
import time

stdscr = curses.initscr()

# Allow color
curses.start_color()

# Disable echoing of input keys
curses.noecho()

# React to keys without Enter key needed
curses.cbreak()

# Allow special keys
stdscr.keypad(1)

# Create a color pair
curses.init_pair(1, curses.COLOR_YELLOW, curses.COLOR_BLACK)

stdscr.addstr("Hello Curses!\n\n", curses.A_BOLD)
stdscr.addstr("Hit 'q' to quit.", curses.color_pair(1) | curses.A_BOLD)
stdscr.refresh()

while True:
  c = stdscr.getch()
  if c == ord('q'): break

curses.endwin()
