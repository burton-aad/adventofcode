#!/usr/bin/env python3

import sys
import re

def validate(rule, v):
    (a, b), (c, d) = rule
    return a <= v <= b or c <= v <= d

def part1(rules, nearby):
    v = []
    for near in nearby:
        v += [n for n in near if not any(validate(r, n) for r in rules.values())]
    print("Part 1:", sum(v))

def part2(rules, nearby, ticket):
    # filter invalid
    nearby = [near for near in nearby if all(any(validate(r, n) for r in rules.values()) for n in near)]

    # possibilities per position
    possible = [ { r for r, rng in rules.items()
                   if all(validate(rng, n) for n in l) }
                 for l in zip(*nearby) ]

    # filter solo possibility over until everything is correct
    s = set(next(iter(p)) for p in possible if len(p) == 1)
    while len(s) < len(ticket):
        for p in possible:
            if len(p) != 1:
                p -= s
                if len(p) == 1:
                    s |= p
    possible = [next(iter(p)) for p in possible]

    v = 1
    for t, p in zip(ticket, possible):
        if p.startswith("departure"):
            v *= t
    print("part 2:", v)

if __name__=="__main__":
    with open(sys.argv[1]) as f:
        r = re.compile(r"(.+): (\d+)-(\d+) or (\d+)-(\d+)")
        rules = {}
        for l in iter(lambda: f.readline().strip(), ""):
            m = r.match(l)
            rules[m.group(1)] = ((int(m.group(2)), int(m.group(3))), (int(m.group(4)), int(m.group(5))))

        f.readline()
        ticket = [int(x) for x in f.readline().strip().split(",")]

        f.readline()
        f.readline()
        nearby = [[int(x) for x in l.strip().split(",")] for l in f]

    part1(rules, nearby)
    part2(rules, nearby, ticket)
