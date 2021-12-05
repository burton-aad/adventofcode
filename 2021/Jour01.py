#!/usr/bin/env python3

import sys, os
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from utils import grouper, sliding_window
from functools import reduce

def main(infile):
    with open(infile) as f:
        vals = [int(l) for l in f]

    print("Part 1: ", sum(a < b for a, b in sliding_window(vals, 2)))
    print("Part 2: ", sum(sum(a) < sum(b) for a, b in sliding_window(sliding_window(vals, 3), 2)))


if __name__=="__main__":
    main(sys.argv[1])
