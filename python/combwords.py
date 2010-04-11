#!/usr/bin/env python
#
# perm.py - print all possible permutations of a word
#
# Copyright 2009 Tobias Klauser <tklauser@distanz.ch>
#
# "THE BEER-WARE LICENSE" (Revision 42):
# <tklauser@distanz.ch> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return
#  Tobias Klauser


from __future__ import generators
import sys, os

def usage():
    print "usage: %s WORD..." % (os.path.basename(sys.argv[0]))

def xcombinations(items, n):
    if n==0:
        yield []
    else:
        for i in xrange(len(items)):
            for cc in xcombinations(items[:i] + items[i+1:], n-1):
                yield [items[i]] + cc

def xpermutations(items):
    return xcombinations(items, len(items))

def main():

    if len(sys.argv[1:]) < 1:
        usage()

    for w in sys.argv[1:]:
        print "Permutations of %s:" % w
        for p in xpermutations(w): print " %s" % ''.join(p)

if __name__=="__main__":
    main()

