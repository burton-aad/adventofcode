#!/usr/bin/env python3

import sys
from functools import reduce
from operator import mul
import argparse


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
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 03')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        area = [l.strip() for l in f]
    v = [Jour03(area, r, d) for r,d in [(3,1), (1,1), (5,1), (7,1), (1,2)]]
    print(f"Part 1: {v[0]}")
    print(f"Part 2: {reduce(mul, v, 1)}")
