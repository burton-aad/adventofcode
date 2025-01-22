#!/usr/bin/env python3

import sys
import re
import argparse

def Jour07(f):
    d = {}
    reverse_d = {}
    regex = re.compile(r"(\d+)? ?(\w+ \w+) bags?")
    for l in f:
        m = regex.finditer(l)
        cont_bag = next(m).group(2)
        for b in m:
            if b.group(2) != "no other":
                d.setdefault(cont_bag, []).append((int(b.group(1)), b.group(2)))
                reverse_d.setdefault(b.group(2), []).append((int(b.group(1)), cont_bag))

    q = ["shiny gold"]
    s = set()
    while len(q) > 0:
        k = q.pop()
        for _, b in reverse_d.get(k, []):
            s.add(b)
            q.append(b)
    print("Part 1:", len(s))

    q = [(1, "shiny gold")]
    r = 0
    while len(q) > 0:
        i, k = q.pop()
        r += i
        for j, b in d.get(k, []):
            q.append((i*j, b))
    print("Part 2:", r-1)

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 07')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        Jour07(f)
