#!/usr/bin/env python

from copy import copy

CHARS = "mohitamrita"

def perm(length, min, max):
  def make_perm(length, num):
    perm = []
    for i in range(0, length):
      perm.append(num)

    return perm

  def permx(length, min, max, state, perms):
    if length == 0:
      perms.append(copy(state))
      return perms

    for c in range(min, max + 1):
      state[length-1] = c
      permx(length-1, min, max, state, perms)

    return perms

  return permx(length, min, max, make_perm(length, 0), [])

"""
def charperm(chars, perms):
  for prm in perms:
    char = ""
    for p in prm:
      char

for i in range(4, 9):
  charperm(CHARS, perm(i, 1, 9))
"""

print perm(3, 1, 9)
