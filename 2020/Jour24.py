#!/usr/bin/env python3

import sys

moves = {
    "e" : complex(2, 0),
    "w" : complex(-2, 0),
    "nw" : complex(-1, 1),
    "ne" : complex(1, 1),
    "se" : complex(1, -1),
    "sw" : complex(-1, -1),
}

def tile_pos(path):
    pos = complex(0, 0)
    it = iter(path)
    for e in it:
        if e == "s" or e == "n":
            e += next(it)
        pos += moves[e]
    return pos

def Jour24(tiles):
    s = set()
    for t in tiles:
        p = tile_pos(t)
        if p in s:
            s.remove(p)
        else:
            s.add(p)
    print("Part 1:", len(s))

    voisins = lambda p: [p+c for c in moves.values()]
    rule = lambda t, s, l: len(l) == 2 or (len(l) == 1 and t in s)
    for i in range(100):
        ns = s.copy()
        for t in s:
            ns.update(voisins(t))
        s = {t for t in ns if rule(t, s, [p for p in voisins(t) if p in s])}
    print("Part 2:", len(s))


if __name__=="__main__":
    with open(sys.argv[1]) as f:
        tiles = [l.strip() for l in f]
    Jour24(tiles)
