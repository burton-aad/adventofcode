#!/usr/bin/python

from __future__ import print_function
from pprint import pprint
from operator import and_

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

    def __repr__(self):
        return "Block({})".format("/".join("".join(x) for x in self.t))

    def __eq__(self, other):
        return reduce(and_, [s == o for l in [zip(x,y) for x,y in zip(self.t, other.t)] for s,o in l])

    def __hash__(self):
        return hash(tuple(self.t))




start_image = ".#./..#/###".split("/")
print()
print("\n".join(start_image))
print(len(start_image))

def divide_in_block(image):
    t = []
    if len(image) % 2 == 0:
        d = 2
    elif len(image) % 3 == 0:
        d = 3

    for i in range(0, len(image), d):
        l = []
        for j in range(0, len(image), d):
            m = []
            for k in range(d):
                m.append(image[i+k][j:j+d])
            l.append(m)
        t.append(l)
    return t


print(divide_in_block(["#..#", "....", "....", "#..#"]))
print(divide_in_block(start_image))

start_image = ".#./..#/###".split("/")
print(hash(tuple(start_image)))
d = tuple(tuple(x) for x in start_image)
print(d)
print(hash(d))

print(d[::-1])
print(zip(*start_image[::-1]))
print(zip(*zip(*start_image)[::-1]))

print("\n".join("1234/5678/90ab/cdef".split("/")))
b = Block.from_line(".#./..#/###")[0]
c = Block.from_line("#../#.#/##.")[0]
# d = Block.from_line("#..#/..../..../#..#")
d = Block.from_line("1234/5678/90ab/cdef")
print("b:", b)
print("c:", c)
print("d:", d)

c.rotate270()
print(c)
print(hash(b))
print(hash(c))
print([(s,o) for l in [zip(x,y) for x,y in zip(b.t, c.t)] for s,o in l])
print(b == c)


img = list("#..#/..../..../#..#".split("/"))

