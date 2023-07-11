#!/usr/bin/python

from __future__ import print_function
import sys
from pprint import pprint
import argparse


up = lambda x, y: (x, y+1)
down = lambda x, y: (x, y-1)
left = lambda x, y: (x-1, y)
right = lambda x, y: (x+1, y)


class Worm:
    directions = [left, up, right, down]

    def __init__(self, grid = {}):
        self.pos = (0, 0)
        self.dirct = 1 # up
        self.grid = grid.copy()
        self.infect = 0

    def wake_up(self):
        if self.pos in self.grid:
            self.grid.pop(self.pos)
            self.dirct = (self.dirct + 1) % len(self.directions)
        else:
            self.grid[self.pos] = '#'
            self.infect += 1
            self.dirct = (self.dirct - 1) % len(self.directions)
        self.pos = self.directions[self.dirct](*self.pos)

    def wake_up2(self):
        s = self.grid.get(self.pos, '.')
        if s == '#':
            self.grid[self.pos] = 'F'
            self.dirct = (self.dirct + 1) % len(self.directions)
        elif s == 'W':
            self.grid[self.pos] = '#'
            self.infect += 1
        elif s == 'F':
            self.grid.pop(self.pos)
            self.dirct = (self.dirct + 2) % len(self.directions)
        elif s == '.':
            self.grid[self.pos] = 'W'
            self.dirct = (self.dirct - 1) % len(self.directions)
        self.pos = self.directions[self.dirct](*self.pos)


def jour22(fname, lnum1=0, lnum2=0):
    w = Worm(grid)
    for _ in range(lnum1):
        w.wake_up()
    # print(w.pos, w.grid)
    print("Part 1:", w.infect)

    w = Worm(grid)
    for _ in range(lnum2):
        w.wake_up2()
    # print(w.pos, w.grid)
    print("Part 2:", w.infect)



if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 22')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        t = [list(l.strip()) for l in f]
    # pprint(t)

    r = len(t)//2
    grid = {}
    for y, l in enumerate(t):
        for x, c in enumerate(l):
            if c == '#':
                grid[(x-r, r-y)] = '#'
    # print(grid)

    jour22(grid, 10_000, 10_000_000)
