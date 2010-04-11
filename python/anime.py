#!/usr/bin/env python

import curses
import time
import math

stdscr = curses.initscr()

# Allow color
curses.start_color()

# Disable echoing of input keys
curses.noecho()

# Make getch() not block
stdscr.nodelay(1)

# React to keys without Enter key needed
curses.cbreak()

# Allow special keys
stdscr.keypad(1)

# Disable cursor
# curses.curs_set(0)

# Create a color pair
curses.init_pair(1, curses.COLOR_YELLOW, curses.COLOR_BLACK)

class Smootherizer(object):
  def common(x):
    return (x * x * (3.0 - 2.0 * x))

  def slow(x):
    return x * x;

  def strange(x):
    return x * x;

  def sin(x):
    return math.sin(x * math.pi / 2)

  def linear(x):
    return x

  common = staticmethod(common)
  slow = staticmethod(slow)
  strange = staticmethod(strange)
  sin = staticmethod(sin)
  linear = staticmethod(linear)

class Anime:
  def __init__(self, from_x, to_x):
    self.from_x = from_x
    self.to_x = to_x

    self.x = 0.0
    self.length_x = to_x - from_x
    self.direction = 1

    self.y = 10
    self.prev_x = 0
    self.pprev_x = 0
    self.interpolation = Smootherizer.common

    self.primary_char = "-"
    self.secondary_char = "."

    self.height = 1

  def smooth_x(self):
    x = self.x
    v = x / self.length_x
    step = self.interpolation(v)
    return int((self.from_x * step) + (self.to_x * (1 - step)))

  def draw(self):
    vertex_a = self.smooth_x()
    vertex_b = self.y

    for h in range(0, self.height):
      stdscr.addstr(vertex_b + h, vertex_a, self.primary_char, curses.A_BOLD)

    if (vertex_a != self.prev_x):
      for h in range(0, self.height):
        stdscr.addstr(vertex_b + h, self.prev_x, self.secondary_char);

    if (self.prev_x != self.pprev_x):
      for h in range(0, self.height):
        stdscr.addstr(vertex_b + h, self.pprev_x, " ");

    self.pprev_x = self.prev_x
    self.prev_x = vertex_a

  def update(self):
    self.draw()
    stdscr.refresh()

    self.x = self.x + self.direction

    if self.x >= self.length_x:
      self.direction = -1

    if self.x <= 0:
      self.direction = 1

def update_status(interpolation):
  stdscr.addstr(0, 0, "Interpolation: " + interpolation + "       ",
                curses.color_pair(1) | curses.A_BOLD)
  stdscr.addstr(1, 0, "(c)ommon (s)low s(t)range si(n) (l)inear (q)uit",
                curses.color_pair(1) | curses.A_BOLD)
  stdscr.refresh()

anime = Anime(10, 70)
c = ""
update_status("common")

while c != ord('q'):
  c = stdscr.getch()
  interpolation = "common"

  if c != -1:
    if c == ord("s"):
      anime.interpolation = Smootherizer.slow
      interpolation = "slow"
    elif c == ord("t"):
      anime.interpolation = Smootherizer.strange
      interpolation = "strange"
    elif c == ord("c"):
      anime.interpolation = Smootherizer.common
      interpolation = "common"
    elif c == ord("n"):
      anime.interpolation = Smootherizer.sin
      interpolation = "sin"
    elif c == ord("l"):
      anime.interpolation = Smootherizer.linear
      interpolation = "linear"

    update_status(interpolation)

  anime.update()
  time.sleep(0.02)

curses.endwin()
