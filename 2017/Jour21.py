#!/usr/bin/python

from __future__ import print_function
import sys
from pprint import pprint
from operator import and_, add

class Block:
    def __init__(self, t):
        self.t = t

    @classmethod
    def from_line(cls, l):
        if len(l) == 5 or len(l) == 11:
            return [cls([tuple(x) for x in l.split("/")])]
        elif len(l) == 19:
            # separate in block of two
            r = []
            # r.append(cls([tuple(l[0:2]), tuple(l[5:7])]))
            for i in range(0, 20, 10):
                for j in range(0, 4, 2):
                    m = []
                    for k in [0,1]:
                        m.append(l[i+j+5*k:i+j+5*k+2])
                    r.append(cls([tuple(x) for x in m]))
            return r
        else:
            raise(Exception("invalid description line {}".format(l)))

    def nop(self):
        pass

    def flip_h(self):
        self.t = self.t[::-1]

    def flip_v(self):
        self.t = zip(*zip(*self.t)[::-1])

    def rotate90(self):
        self.t = zip(*self.t[::-1])

    def rotate180(self):
        self.rotate90()
        self.rotate90()

    def rotate270(self):
        self.rotate180()
        self.rotate90()

    def count(self):
        return sum([x == '#' for t in self.t for x in t])

    def __repr__(self):
        return "Block({})".format("/".join("".join(x) for x in self.t))

    def __len__(self):
        return len(self.t)

    def __eq__(self, other):
        return reduce(and_, [s == o for l in [zip(x,y) for x,y in zip(self.t, other.t)] for s,o in l])

    def __hash__(self):
        return hash(tuple(self.t))

def test():
    print("\n".join("1234/5678/90ab/cdef".split("/")))
    b = Block.from_line(".#./..#/###")[0]
    c = Block.from_line("#../#.#/##.")[0]
    # d = Block.from_line("#..#/..../..../#..#")
    d = Block.from_line("1234/5678/90ab/cdef")
    print("b {}: {}".format(len(b), b))
    print(b.count())
    print("c {}: {}".format(len(c), c))
    print(c.count())
    print("d {}: {}".format(len(d[0]), d[0]))

    c.rotate270()
    print(c)
    print(hash(b))
    print(hash(c))
    print([(s,o) for l in [zip(x,y) for x,y in zip(b.t, c.t)] for s,o in l])
    print(b == c)


start_image = ".#./..#/###"

def find_replace(block, dic):
    for f in [Block.nop, Block.flip_h, Block.flip_v]:
        f(block)
        for g in [Block.rotate90] * 4:
            g(block)
            if block in dic:
                print(block)
                return dic[block]
        f(block)
    raise(Exception("Something is wrong here"))

def jour21(filename, loop=3):
    d = {}
    with open(filename) as f:
        for l in f:
            k,v = l.strip().split(" => ")
            d[Block.from_line(k)[0]] = Block.from_line(v)
    # pprint(d)
    lb = [Block.from_line(start_image)[0]]
    for _ in range(loop):
        nl = []
        for b in lb:
            nl.extend(find_replace(b, d))
        lb = nl
    print("lb {} : {}".format(len(lb), lb))
    print(list(map(len, lb)))
    print(list(map(Block.count, lb)))
    print(sum(map(Block.count, lb)))

if __name__=="__main__":
    fname = "input21"
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    # test()
    jour21(fname, 1)
    jour21(fname, 2)
    jour21(fname, 3)
    # jour21(fname, 4)
    # jour21(fname, 5)
