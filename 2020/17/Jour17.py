#!/usr/bin/env python3

import sys
import itertools

def minmaxkey(iterable, key=None):
    if key is None:
        it = iter(iterable)
    else:
        it = (key(x) for x in iterable)
    min = max = next(it)
    for v in it:
        if v < min:
            min = v
        if v > max:
            max = v
    return min, max

def tadd(x, y):
    return tuple(a+b for a,b in zip(x, y))

def univers(state, l=3, offset = (-1, 2)):
    return (range(*tadd(minmaxkey(state, key=lambda x: x[i]), offset)) for i in range(l))

def print_state(state, z=None):
    xr, yr, zr = univers(state, offset=(0,1))
    if z != None:
        zr = [z]
    for z in zr:
        print("z={}".format(z))
        print("\n".join(["".join("#" if (x,y,z) in state else "."
                                 for y in yr)
                         for x in xr]))
        print()

def voisinsND(pos):
    return (tadd(pos, v) for v in itertools.product(range(-1, 2), repeat=len(pos)) if v != (0,) * len(pos))

def is_active(state, pos):
    c = len([t for t in voisinsND(pos) if t in state])
    return c == 3 or (pos in state and c == 2)

def run(state, l=3):
    new_state = set()
    for pos in itertools.product(*univers(state, l)):
        if is_active(state, pos):
            new_state.add(pos)
    return new_state

if __name__=="__main__":
    with open(sys.argv[1]) as f:
        init_state = { (x, y) for x, l in enumerate(f) for y, v in enumerate(l) if v == "#" }

    state = { (x, y, 0) for x, y in init_state }
    for i in range(6):
        state = run(state)
    print("Part 1:", len(state))

    state = { (x, y, 0, 0) for x, y in init_state }
    for i in range(6):
        state = run(state, 4)
    print("Part 2:", len(state))
