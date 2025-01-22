#!/usr/bin/env python3

import sys
import argparse
from functools import reduce

def Jour06(f):
    r1, r2 = 0, 0
    while (s := [set(l) for l in iter(lambda: f.readline().strip(), "")]) != []:
        r1 += len(reduce(lambda x, y: x | y, s))
        r2 += len(reduce(lambda x, y: x & y, s))
    print(f"Part 1 : {r1}")
    print(f"Part 2 : {r2}")

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 06')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        Jour06(f)
