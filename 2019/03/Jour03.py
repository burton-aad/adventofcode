#!/usr/bin/env python

from __future__ import print_function
import sys
import argparse

class Position(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def distance(self, to):
        return abs(self.x - to[0]) + abs(self.y - to[1])

    def __add__(self, inc):
        return Position(self.x + inc[0], self.y + inc[1])

    def __sub__(self, inc):
        return Position(self.x - inc[0], self.y - inc[1])

    def __getitem__(self, index):
        return (self.x, self.y)[index]

    def __eq__(self, o):
        return self.x == o[0] and self.y == o[1]

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return "P({}, {})".format(self.x, self.y)


class Wire(object):
    def __init__(self):
        self.pos = Position(0, 0)
        self.s = []

    def parse_path(self, path):
        for p in path.split(","):
            d = p[0]
            l = int(p[1:])
            if d == "R":
                for i in range(l):
                    self.s.append(self.pos + (i+1,0))
                self.pos += (l,0)
            elif d == "D":
                for i in range(l):
                    self.s.append(self.pos - (0,i+1))
                self.pos -= (0,l)
            elif d == "U":
                for i in range(l):
                    self.s.append(self.pos + (0,i+1))
                self.pos += (0,l)
            elif d == "L":
                for i in range(l):
                    self.s.append(self.pos - (i+1,0))
                self.pos -= (l,0)


def main(path1, path2):
    w1 = Wire()
    w2 = Wire()
    w1.parse_path(path1)
    w2.parse_path(path2)
    inter = set(w1.s) & set(w2.s)
    # print(inter)
    print("part 1 :", min([x.distance((0,0)) for x in inter]))
    print("part 2 :", min([(w1.s.index(x)+1 + w2.s.index(x)+1) for x in inter]))

# main("R8,U5,L5,D3", "U7,R6,D4,L4")
# main("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
# main("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2019 - Jour 03')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        p1, p2 = f.readlines()
    main(p1.strip(), p2.strip())
