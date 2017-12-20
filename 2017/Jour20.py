#!/usr/bin/python

from __future__ import print_function
from operator import sub
from pprint import pprint
import re
import sys

sign = lambda x: x and (1, -1)[x < 0]

class Particule:
    def __init__(self, index=-1, pos=[0,0,0], v=[0,0,0], a=[0,0,0]):
        self.index = index
        self.pos = list(pos)
        self.v = list(v)
        self.a = list(a)

    def from_desc(self, line):
        p, v, a = re.findall("-?\d[-\d,]*", line)
        self.pos = map(int, p.split(","))
        self.v = map(int, v.split(","))
        self.a = map(int, a.split(","))
        return self

    def update(self):
        self.v = map(sum, zip(self.v, self.a))
        self.pos = map(sum, zip(self.pos, self.v))

    def dist(self):
        return sum(map(abs, self.pos))

    def __repr__(self):
        return "({}: p {}, v {}, a {})".format(self.index, self.pos, self.v, self.a)

    def __eq__(self, other):
        return self.pos[0] == other.pos[0] and self.pos[1] == other.pos[1] and self.pos[2] == other.pos[2]

def search_duplicate(parts):
    t = []
    d = {}
    for p in parts:
        if p.pos in t:
            l = tuple(p.pos)
            d[l] = d.get(l, 1) + 1
        else:
            t.append(p.pos)
    return d

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
        print(parts)
        # map(lambda (x,y): print("{} -> {}".format(x, y.dist()), end=" "), enumerate(parts))
        # print()
        map(Particule.update, parts)
    map(lambda (x,y): print("{} -> {}".format(x, y.dist())), enumerate(parts))

def jour20(filename):
    with open(filename) as f:
        parts = []
        for i, l in enumerate(f):
            parts.append(Particule(i).from_desc(l))

    p = min(parts, key=lambda x: sum(map(abs, x.a)))
    print("Cas 1: closest index {} -> {}".format(p.index, p.dist()))

    for i in range(150):
        map(Particule.update, parts)
        d = search_duplicate(parts)
        if len(d) != 0:
            # print("loop", i)
            for p in d:
                # print("remove {} collision at pos {}".format(d[p], p))
                for _ in range(d[p]):
                    parts.remove(Particule(pos=p))
    print()
    print("Cas 2:", len(parts))
    # pprint(parts)


if __name__=="__main__":
    fname = "input20"
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    # test()
    jour20(fname)
