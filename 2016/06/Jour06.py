#!/usr/bin/env python3

import argparse

from collections import Counter

def max_letter(t):
    c = Counter(t)
    return c.most_common(1)[0][0]

def min_letter(t):
    c = Counter(t)
    return c.most_common()[-1][0]


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 06')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        m = list(zip(*f.readlines()))[:-1]

    print("Part 1:", "".join(max_letter(l) for l in m))
    print("Part 2:", "".join(min_letter(l) for l in m))
