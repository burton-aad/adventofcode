#!/usr/bin/python

from __future__ import print_function
import sys
import argparse
from pprint import pprint


class Bridge:
    def __init__(self, start=0):
        self.strength = 0
        self.pts = set()
        self.pos = start

    def copy(self):
        b = Bridge()
        b.strength = self.strength
        b.pts = self.pts.copy()
        b.pos = self.pos
        return b

    def set_next_point(self, i, j):
        self.strength += i+j
        self.pts.add((i,j))
        self.pos = i if self.pos == j else j

    def next(self, G):
        r = []
        s = [(i,j) for i,j in G - self.pts if i == self.pos or j == self.pos]
        for v in s[:-1]:
            b = self.copy()
            b.set_next_point(*v)
            r.append(b)
        if len(s) > 0:
            # last
            self.set_next_point(*s[-1])
            r.append(self)
        return r

    def __repr__(self):
        return "{} : {}".format(self.strength, self.pts)

def jour24(G):
    bgs = []
    fifo = [Bridge()]
    while len(fifo) > 0:
        b = fifo.pop(0)
        nbs = b.next(G)
        if nbs == []:
            bgs.append(b)
        else:
            fifo.extend(nbs)
    # pprint(bgs)

    print("Part 1:", max(bgs, key=lambda x: x.strength))

    m = max(bgs, key=lambda x: len(x.pts))
    m = max(filter(lambda x: len(x.pts) == len(m.pts), bgs), key=lambda x: x.strength)
    print("Part 2:", m)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 24')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        G = {tuple(map(int, l.strip().split("/"))) for l in f}
    pprint(G)

    jour24(G)
