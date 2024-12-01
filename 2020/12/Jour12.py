#!/usr/bin/env python3

import sys

class C:
    def __init__(self, r, i):
        self.c = complex(r, i)

    def man_dist(self):
        return int(abs(self.c.real) + abs(self.c.imag))

    def move(self, c):
        self.c += c

    def turn(self, direction, num):
        # 90 degrees turn, direction 1 for right and -1 for left
        for _ in range(num):
            self.c = complex(direction * self.c.imag, -direction * self.c.real)

def Jour12_1(insts):
    pos = C(0, 0)
    face = C(1, 0) # face east by default
    action = {
        'N' : lambda x: pos.move(complex(0, 1) * x),
        'S' : lambda x: pos.move(complex(0, -1) * x),
        'E' : lambda x: pos.move(complex(1, 0) * x),
        'W' : lambda x: pos.move(complex(-1, 0) * x),
        'F' : lambda x: pos.move(face.c * x),
        'L' : lambda x: face.turn(-1, x // 90),
        'R' : lambda x: face.turn(1, x // 90),
    }

    for i, v in insts:
        action[i](v)
    print("Part 1 :", pos.man_dist())

def Jour12_2(insts):
    pos = C(0, 0)
    waypoint = C(10, 1)
    action = {
        'N' : lambda x: waypoint.move(complex(0, 1) * x),
        'S' : lambda x: waypoint.move(complex(0, -1) * x),
        'E' : lambda x: waypoint.move(complex(1, 0) * x),
        'W' : lambda x: waypoint.move(complex(-1, 0) * x),
        'F' : lambda x: pos.move(waypoint.c * x),
        'L' : lambda x: waypoint.turn(-1, x // 90),
        'R' : lambda x: waypoint.turn(1, x // 90),
    }

    for i, v in insts:
        action[i](v)
    print("Part 2 :", pos.man_dist())


if __name__=="__main__":
    with open(sys.argv[1]) as f:
        insts = [(l[0], int(l[1:])) for l in f]
    Jour12_1(insts)
    Jour12_2(insts)
