#!/usr/bin/env python3

import sys
from functools import reduce
from operator import mul


def Jour03(area, r, d):
    x, y = 0, 0
    w = len(area[0])
    tree = 0
    while y < len(area):
        tree += area[y][x] == "#"
        x = (x + r) % w
        y += d
    return tree


if __name__=="__main__":
    with open(sys.argv[1]) as f:
        area = [l.strip() for l in f]
    v = [Jour03(area, r, d) for r,d in [(3,1), (1,1), (5,1), (7,1), (1,2)]]
    print(f"Part 1: {v[0]}, Part 2: {reduce(mul, v, 1)}")
