#!/usr/bin/env python3

import sys
import re
import collections
import argparse
from functools import reduce

def Jour21(foods):
    words = collections.Counter()
    allergenes = {}
    for w, algs in foods:
        words.update(w)
        for a in algs:
            allergenes.setdefault(a, set(w)).intersection_update(w)

    inert = set(words) - reduce(lambda x, y: x | y, allergenes.values())
    print("Part 1:", sum(words[w] for w in inert))

    while len(inert) < len(words):
        for a, l in allergenes.items():
            if len(l) > 1:
                l -= inert
            if len(l) == 1:
                inert |= l
    for a, l in allergenes.items():
        allergenes[a] = next(iter(l))
    print("Part 2:", ",".join(allergenes[a] for a in sorted(allergenes)))


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 21')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        foods = []
        for l in f:
            l = re.findall(r"\w+", l)
            i = l.index('contains')
            foods.append((l[:i], l[i+1:]))
    Jour21(foods)
