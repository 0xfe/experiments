#!/usr/bin/env python

from os.path import basename
from sys import argv, exit


def main(word):
    print 'Reordering: ' + word
    chars = list(word)
    chars.sort()
    print 'Chars: %s (%d)' % (''.join(chars), len(chars))
    words = swap(chars)
    # Filter duplicates and sort.
    # words = sorted(set(words))
    # words = set(words)
    for word in words:
        print word

def swap(remaining, ordered=[]):
    """Recursively swap characters."""
    for i in xrange(len(remaining)):
        remaining2 = remaining[:]
        ordered2 = ordered + [remaining2.pop(i)]
        if len(remaining2) > 0:
            for word in swap(remaining2, ordered2):
                yield word
        else:
            yield ''.join(ordered2)

if __name__ == '__main__':
    if len(argv) != 2:
        print 'Usage: %s <word>' % basename(argv[0])
        exit(2)
    main(argv[1])
