#!/usr/bin/env python3

import sys
import argparse
from functools import reduce

def part1(depart, ids):
    _, id = min(ids, key=lambda x: x[1] - depart % x[1])
    print("part 1 :", (id - depart % id) * id)

def euclide_etendu(a, b):
    r, u, v, rp, up, vp = a, 1, 0, b, 0, 1
    while rp != 0:
        q = r // rp
        r, u, v, rp, up, vp = rp, up, vp, r - q *rp, u - q*up, v - q*vp
    return r, u, v

def th_chinois(vals):
    # vals form : (reste, mod)
    n = reduce(lambda x,y : x*y[1], vals, 1)
    E = []
    for i, p in vals:
        np = n // p
        _, v, _ = euclide_etendu(np, p)
        E.append((v % p) * np)
    return sum(a*e for (a, _),e in zip(vals, E)) % n

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 13')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        depart = int(f.readline())
        ids = [(int(t)-i, int(t)) for i, t in enumerate(f.readline().strip().split(",")) if t != "x"]
    part1(depart, ids)
    print("part 2 :", th_chinois(ids))
