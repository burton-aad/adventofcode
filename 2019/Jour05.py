#!/usr/bin/env python

from __future__ import print_function
import sys
from intcomp import Intcode


def main(v):
    p = Intcode(v)
    print("Part 1 :")
    p.run(1)
    p.reset()
    print("\nPart 2 :")
    p.run(5)


# main([1002, 4, 3, 4, 33])
# main([1101, 100, -1, 4, 0])


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage : {} <input>".format(sys.argv[0]))
        sys.exit(1)
    with open(sys.argv[1]) as f:
        v = [int(x) for x in f.read().strip().split(',')]
        main(v)
