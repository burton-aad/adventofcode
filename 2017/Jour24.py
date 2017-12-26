#!/usr/bin/python

from __future__ import print_function
import sys
from pprint import pprint


class Bridge:
    def __init__(self, start=0):
        self.strength = 0
        self.pts = []
        self.pos = start

    def copy(self):
        b = Bridge()
        b.strength = self.strength
        b.pts = self.pts[:]
        b.pos = self.pos
        return b

    def set_next_point(self, i, j):
        self.strength += i+j
        self.pts.append((i,j))
        self.pos = i if self.pos == j else j

    def next(self, G):
        r = []
        for i,j in G - set(self.pts):
            if i == self.pos or j == self.pos:
                b = self.copy()
                b.set_next_point(i, j)
                r.append(b)
        return r

    def __repr__(self):
        return "{} : {}".format(self.strength, self.pts)

def jour24(filename):
    G = set()
    with open(filename) as f:
        for l in f:
            i,j = map(int, l.strip().split("/"))
            G.add((i,j))
    pprint(G)

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

    print("Cas 1:", max(bgs, key=lambda x: x.strength))

    m = max(bgs, key=lambda x: len(x.pts))
    m = max(filter(lambda x: len(x.pts) == len(m.pts), bgs), key=lambda x: x.strength)
    print("Cas 2:", m)

if __name__ == "__main__":
    fname = "input24"
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    jour24(fname)
