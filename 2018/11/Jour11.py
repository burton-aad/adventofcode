#!/usr/bin/python

from __future__ import print_function
import sys
import argparse


def power_lvl(x, y, serial):
    rackId = x + 10
    plvl = (rackId * y + serial) * rackId
    return ((plvl % 1000) // 100) - 5

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
    p1 = g.find_max()[::-1]
    print("region {} with power {}".format(*p1))
    print("Part 1: {},{}".format(*p1[0]))
    p2 = [0]
    size = 0
    for n in range(300):
        t = g.find_max((n+1, n+1))
        print(t[0])
        if t[0] == 0:
            break
        if t[0] > p2[0]:
            p2 = t
            size = n+1
    print("largest: region {}, size {}, with power {}".format(p2[1], size, p2[0]))
    print("Part 2: {},{},{}".format(*p2[1], size))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 11')
    parser.add_argument("serial", nargs='?', type=int, default=5034, help="grid serial number")
    args = parser.parse_args()
    jour11(args.serial)
