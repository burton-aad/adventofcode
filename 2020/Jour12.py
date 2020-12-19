#!/usr/bin/env python3

import sys


def Jour12_1(insts):
    pos = complex(0,0)
    def move(v):
        nonlocal pos
        pos += v

    face = 'E'
    direction = ['N', 'E', 'S', 'W']
    def turn(degree):
        nonlocal face
        face = direction[(direction.index(face) + degree // 90 + 4) % 4]

    action = {
        'N' : lambda x: move(complex(0, 1) * x),
        'S' : lambda x: move(complex(0, -1) * x),
        'E' : lambda x: move(complex(1, 0) * x),
        'W' : lambda x: move(complex(-1, 0) * x),
        'F' : lambda x: action[face](x),
        'L' : lambda x: turn(-x),
        'R' : lambda x: turn(x),
    }

    for i, v in insts:
        action[i](v)
    print("Part 1 :", abs(pos.real) + abs(pos.imag))

def Jour12_2(insts):
    pos = complex(0,0)
    def move(v):
        nonlocal pos
        pos += v

    waypoint = complex(10, 1)
    def move_w(v):
        nonlocal waypoint
        waypoint += v

    def turn(d, degree):
        nonlocal waypoint
        for _ in range(degree // 90):
            waypoint = complex(d * waypoint.imag, -d * waypoint.real)

    action = {
        'N' : lambda x: move_w(complex(0, 1) * x),
        'S' : lambda x: move_w(complex(0, -1) * x),
        'E' : lambda x: move_w(complex(1, 0) * x),
        'W' : lambda x: move_w(complex(-1, 0) * x),
        'F' : lambda x: move(waypoint * x),
        'L' : lambda x: turn(-1, x),
        'R' : lambda x: turn(1, x),
    }

    for i, v in insts:
        action[i](v)
    print("Part 1 :", abs(pos.real) + abs(pos.imag))


if __name__=="__main__":
    with open(sys.argv[1]) as f:
        insts = [(l[0], int(l[1:])) for l in f]
    Jour12_1(insts)
    Jour12_2(insts)
