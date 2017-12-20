#!/usr/bin/python

from __future__ import print_function
from pprint import pprint
import re
import sys

# function compatible pour 2 et 3
mapl = lambda *x: list(map(*x))

class Particule:
    def __init__(self, index=-1, pos=[0,0,0], v=[0,0,0], a=[0,0,0]):
        self.index = index
        self.pos = list(pos)
        self.v = list(v)
        self.a = list(a)

    def from_desc(self, line):
        p, v, a = re.findall("-?\d[-\d,]*", line)
        self.pos = mapl(int, p.split(","))
        self.v = mapl(int, v.split(","))
        self.a = mapl(int, a.split(","))
        return self

    def update(self):
        self.v = mapl(sum, zip(self.v, self.a))
        self.pos = mapl(sum, zip(self.pos, self.v))

    def dist(self):
        return sum(map(abs, self.pos))

    def __repr__(self):
        return "({}: p {}, v {}, a {})".format(self.index, self.pos, self.v, self.a)

    def __eq__(self, other):
        return self.pos[0] == other.pos[0] and self.pos[1] == other.pos[1] and self.pos[2] == other.pos[2]

    def __hash__(self):
        return hash(tuple(self.pos))

def search_and_remove_duplicate(parts):
    t = set()
    d = {}
    for p in parts:
        if p in t:
            d[p] = d.get(p, 1) + 1
        else:
            t.add(p)
    return list(t - set(d))

def test():
    text = ["p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
            "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
            "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
            "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"]
    parts = []
    for i, l in enumerate(text):
        parts.append(Particule(i).from_desc(l))
    for i in range(4):
        print("loop", i)
        parts = search_and_remove_duplicate(parts)
        print(parts)
        # map(lambda (x,y): print("{} -> {}".format(x, y.dist()), end=" "), enumerate(parts))
        # print()
        mapl(Particule.update, parts)
    mapl(lambda t: print("{} -> {}".format(t[0], t[1].dist())), enumerate(parts))

def jour20(filename):
    with open(filename) as f:
        parts = []
        for i, l in enumerate(f):
            parts.append(Particule(i).from_desc(l))

    p = min(parts, key=lambda x: sum(map(abs, x.a)))
    print("Cas 1: closest index {} -> {}".format(p.index, p.dist()))

    for i in range(150):
        mapl(Particule.update, parts)
        parts = search_and_remove_duplicate(parts)
    print()
    print("Cas 2:", len(parts))
    # pprint(parts)


if __name__=="__main__":
    fname = "input20"
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    # test()
    jour20(fname)
