#!/usr/bin/env python

from __future__ import print_function
import sys, os
import argparse
from intcomp import Intcode


def main(v):
    p = Intcode(v)
    r = p.run(1, capture_out=True)
    print("Part 1 :", r[-1])
    p.reset()
    r = p.run(5, capture_out=True)
    print("Part 2 :", r[-1])


# main([1002, 4, 3, 4, 33])
# main([1101, 100, -1, 4, 0])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2019 - Jour 05')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        v = [int(x) for x in f.read().strip().split(',')]
        main(v)
