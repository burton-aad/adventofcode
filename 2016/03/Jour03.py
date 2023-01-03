#!/usr/bin/env python3

import sys, os
import argparse
from itertools import chain
from utils import grouper


def is_valid_triangle(a, b, c):
    return a+b > c and b+c > a and a+c > b

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 03')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        triangles = [list(map(int, l.split())) for l in f]

    print("Part 1: ", sum(is_valid_triangle(*t) for t in triangles))
    print("Part 2: ", sum(is_valid_triangle(*t) for t in grouper(chain(*zip(*triangles)), 3)))
