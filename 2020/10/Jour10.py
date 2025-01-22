#!/usr/bin/env python3

import sys
import argparse
from itertools import tee, groupby
from collections import Counter

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)

def Jour10(joltage):
    j = Counter(y-x for x,y in pairwise(joltage))
    print("Part 1:", j[1] * j[3])

    # c'est pas correcte mais ça marche :(
    s = [sum(x <= jol+3 for x in joltage[i+1:i+4]) for i, jol in enumerate(joltage[:-1])]
    r = 1
    last = 1
    for i in s:
        if i != 3:
            r *= i + last - 1
            last = 1
        else:
            if last == 1:
                last = i
            else:
                last += i
    print("Part 2:", r)


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 10')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        joltage = sorted(int(l) for l in f)
    Jour10([0] + joltage  + [joltage[-1]+3])
