#!/usr/bin/env python3

import sys
import argparse
from itertools import product

def voisins1(seats, i, j):
    return [seats[w][h]
            for w,h in [(i-1,j-1), (i-1,j), (i-1,j+1), (i,j-1), (i,j+1), (i+1,j-1), (i+1,j), (i+1,j+1)]
            if 0 <= w < len(seats) and 0 <= h < len(seats[0])]

def voisins2(seats, i, j):
    r = []
    for di, dj in [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]:
        w, h = i + di, j + dj
        while 0 <= w < len(seats) and 0 <= h < len(seats[0]) and seats[w][h] == ".":
            w, h = w + di, h + dj
        r.append(seats[w][h] if 0 <= w < len(seats) and 0 <= h < len(seats[0]) else ".")
    return r

def rules(state, neighbors, max_ = 4):
    if state == ".":
        return "."
    elif state == "L" and neighbors.count("#") == 0:
        return "#"
    elif state == "#" and neighbors.count("#") >= max_:
        return "L"
    else:
        return state

def run(seats, rules, voisins):
    return ["".join(rules(s, voisins(seats, i, j)) for j, s in enumerate(l)) for i, l in enumerate(seats)]

def Jour11(seats):
    last, r = None, seats
    while last != r:
        last, r = r, run(r, rules, voisins1)
    print("Part 1:", sum(l.count("#") for l in r))

    last, r = None, seats
    while last != r:
        last, r = r, run(r, lambda x,y: rules(x, y, 5), voisins2)
    print("Part 2:", sum(l.count("#") for l in r))

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 11')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        seats = [l.strip() for l in f]
    Jour11(seats)
