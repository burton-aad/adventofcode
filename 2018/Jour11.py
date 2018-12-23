#!/usr/bin/python

from __future__ import print_function
import sys

def power_lvl(x, y, serial):
    rackId = x + 10
    plvl = (rackId * y + serial) * rackId
    return ((plvl % 1000) / 100) - 5

class Grid:
    def __init__(self, serial, w=300, h=300):
        self.serial = serial
        self.w = w
        self.h = h
        self.cells = [[power_lvl(x+1, y+1, self.serial) for x in range(w)] for y in range(h)]

    def find_max(self, kernel=(3,3)):
        print(kernel)
        kw, kh = kernel
        r = 0
        p = (0, 0)
        for y in range(self.h - kh + 1):
            for x in range(self.w - kw + 1):
                s = sum([sum(t[x:x+kw]) for t in self.cells[y:y+kh]])
                # s = sum([self.cells[y+i][x+j] for i in range(kh) for j in range(kw)])
                if r < s:
                    r = s
                    p = (x+1, y+1)
        return r, p

    def __str__(self):
        return "{}".format(self.cells)


def jour11(grid_serial):
    g = Grid(grid_serial)
    print("part 1: region {} with power {}".format(*g.find_max()[::-1]))
    t = [g.find_max((n+1, n+1)) for n in range(300)]
    p2 = max(t, key=lambda x: x[0])
    print("part 2: region {}, size {}, with power {}".format(p2[1], t.index(p2)+1, p2[0]))


if __name__ == "__main__":
    input = 5034
    if len(sys.argv) > 1:
        input = int(sys.argv[1])
    jour11(input)
