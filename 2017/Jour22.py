#!/usr/bin/python

from __future__ import print_function
import sys
from pprint import pprint

def update_pos(i,j,x,y):
    return (x+i, y+j)

up = lambda x: update_pos(0,1, *x)
down = lambda x: update_pos(0,-1, *x)
left = lambda x: update_pos(-1,0, *x)
right = lambda x: update_pos(1,0, *x)


class Worm:
    directions = [left, up, right, down]

    def __init__(self, grid = {}):
        self.pos = (0, 0)
        self.dirct = up
        self.grid = grid.copy()
        self.infect = 0


    def wake_up(self):
        if self.pos in self.grid:
            self.grid.pop(self.pos)
            self.dirct = self.directions[(self.directions.index(self.dirct) + 1) % len(self.directions)]
        else:
            self.grid[self.pos] = '#'
            self.infect += 1
            self.dirct = self.directions[(self.directions.index(self.dirct) - 1) % len(self.directions)]
        self.pos = self.dirct(self.pos)

    def wake_up2(self):
        s = self.grid.get(self.pos, '.')
        if s == '#':
            self.grid[self.pos] = 'F'
            self.dirct = self.directions[(self.directions.index(self.dirct) + 1) % len(self.directions)]
        elif s == 'W':
            self.grid[self.pos] = '#'
            self.infect += 1
        elif s == 'F':
            self.grid.pop(self.pos)
            self.dirct = self.directions[(self.directions.index(self.dirct) + 2) % len(self.directions)]
        elif s == '.':
            self.grid[self.pos] = 'W'
            self.dirct = self.directions[(self.directions.index(self.dirct) - 1) % len(self.directions)]
        self.pos = self.dirct(self.pos)


def jour22(fname, lnum1=0, lnum2=0):
    t = []
    with open(fname) as f:
        for l in f:
            t.append(list(l.strip()))
    # pprint(t)

    r = len(t)/2
    grid = {}
    for y, l in enumerate(t):
        for x, c in enumerate(l):
            if c == '#':
                grid[(x-r, r-y)] = '#'
    # print(grid)

    w = Worm(grid)
    for _ in range(lnum1):
        w.wake_up()
    # print(w.pos, w.grid)
    print("Cas 1:", w.infect)

    w = Worm(grid)
    for _ in range(lnum2):
        w.wake_up2()
    # print(w.pos, w.grid)
    print("Cas 2:", w.infect)




if __name__=="__main__":
    f = "input22"
    if len(sys.argv) > 1:
        f = sys.argv[1]
    jour22(f, 10000, 10000000)
