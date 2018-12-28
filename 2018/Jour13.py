#!/usr/bin/python

from __future__ import print_function
import sys
from operator import attrgetter

class Cart:
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def __init__(self, pos, face):
        self.posx, self.posy = pos
        self.moves = [self.move_up, self.move_right, self.move_down, self.move_left]
        self.face = face
        self.move = self.moves[face]
        self.t = -1

    def move_up(self):
        self.posy -= 1

    def move_down(self):
        self.posy += 1

    def move_left(self):
        self.posx -= 1

    def move_right(self):
        self.posx += 1

    def vertical_move(self):
        return (self.face % 2) == 0

    def turn_t(self):
        self.turn(self.t)
        self.t += 1
        if self.t == 2:
            self.t = -1

    def turn(self, val):
        self.face = (self.face + val) % 4
        self.move = self.moves[self.face]

    def pos(self):
        return (self.posx, self.posy)

    def __str__(self):
        return "{}".format(self.pos())

    def __repr__(self):
        return "{}".format(self.pos())


def tick(carts, tracks):
    l = []
    for i,c in enumerate(carts):
        c.move()
        f = filter(lambda x: x.pos() == c.pos(), carts)
        if len(f) > 1:
            l.extend(f)
    for c in carts:
        if c.pos() in tracks:
            tracks[c.pos()](c)
    carts.sort(key = attrgetter('posy', 'posx'))
    return l


def jour13(f):
    tracks = {}
    carts = []
    for j,l in enumerate(f):
        for i,c in enumerate(l):
            if c == "/":
                tracks[(i,j)] = lambda x: x.turn(1) if x.vertical_move() else x.turn(-1)
            elif c == "\\":
                tracks[(i,j)] = lambda x: x.turn(-1) if x.vertical_move() else x.turn(1)
            elif c == "+":
                tracks[(i,j)] = lambda x: x.turn_t()
            elif c == ">":
              carts.append(Cart((i,j), Cart.RIGHT))
            elif c == "<":
              carts.append(Cart((i,j), Cart.LEFT))
            elif c == "^":
              carts.append(Cart((i,j), Cart.UP))
            elif c == "v":
              carts.append(Cart((i,j), Cart.DOWN))
    print(carts)
    p1 = True
    while len(carts) > 1:
        l = tick(carts, tracks)
        # print(carts)
        if l:
            if p1:
                print("Part 1:", l[0])
                p1 = False
            for c in l:
                carts.remove(c)
    print("Part 2:", carts[0])


if __name__ == "__main__":
    input = "input13"
    if len(sys.argv) > 1:
        input = sys.argv[1]
    with open(input) as f:
        jour13(f)

