#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
from collections import deque


def vois4(p):
    i,j = p
    return [(i,j-1), (i-1,j), (i+1,j), (i,j+1)]

def readOrderCmp(x, y):
    i = x[1] - y[1]
    if i == 0:
        return x[0] - y[0]
    else:
        return i


def print_map_debug(mp, plyrs, pos, r):
    for j,l in enumerate(mp):
        t = []
        tl = []
        for i,c in enumerate(l):
            if (i,j) in plyrs:
                t.append(plyrs[(i,j)].ptype)
                tl.append(plyrs[(i,j)].plife())
            elif (i,j) in pos and (i,j) in r:
                t.append('J')
            elif (i,j) in pos:
                t.append('O')
            elif (i,j) in r:
                t.append('X')
            else:
                t.append('.' if c else '#')
        print("".join(t), ", ".join(tl))



class PNJ:
    def __init__(self, ptype, pos, hp = 200, ap = 3):
        self.pos = pos
        self.ap = ap
        self.hp = hp
        self.ptype = ptype

    def move(self, carte, players):
        # print("move", self)
        q = [] # liste à faire
        qr = [] # prochaine liste
        found = []
        r = set([self.pos])

        # check if opponent in range
        for v in vois4(self.pos):
            if v in players:
                if players[v].ptype != self.ptype:
                    # can attack opponent, no move
                    return False
            elif carte[v[1]][v[0]]:
                q.append((v, v))

        # q is list of (first move, actual position)
        while not found and len(q) > 0:
            # print(len(q), ":", q)
            # print(r)
            # print_map_debug(carte, players, [x[1] for x in q], r)
            for p in q:
                move, cur = p
                # print(cur)
                # if cur in r:
                #     print("cur in r")
                #     continue
                r.add(cur)
                for v in vois4(cur):
                    # print("vois", v, "->", carte[v[1]][v[0]])
                    if v in r:
                        # print("in r")
                        pass
                    elif v in players:
                        # print("in players")
                        if players[v].ptype != self.ptype:
                            found.append((move, cur))
                    elif carte[v[1]][v[0]]:
                        qr.append((move, v))
                        r.add(v)
            q = sorted(qr, cmp=readOrderCmp, key=lambda x: x[1])
            qr = []
        # print(found)
        if found:
            self.pos, _ = sorted(found, cmp=readOrderCmp, key=lambda x: x[1])[0]
            return True
        return False

    def attack(self, players):
        p = sorted(filter(lambda x: x and x.ptype != self.ptype, map(players.get, vois4(self.pos))), key=lambda x: x.hp)
        if len(p) > 0:
            p[0].hp -= self.ap
            if p[0].hp <= 0:
                players.pop(p[0].pos)

    def plife(self):
        "return string with player hp"
        return "{}({})".format(self.ptype, self.hp)

    def __repr__(self):
        return "{}({},{})".format(self.ptype, self.pos, self.hp)

    def __getitem__(self, i):
        return self.pos[i]

    def __cmp__(self, o):
        return readOrderCmp(self, o)

    def __hash__(self):
        return hash(self.pos)


def print_map(mp, plyrs):
    for j,l in enumerate(mp):
        t = []
        tl = []
        for i,c in enumerate(l):
            if (i,j) in plyrs:
                t.append(plyrs[(i,j)].ptype)
                tl.append(plyrs[(i,j)].plife())
            else:
                t.append('.' if c else '#')
        print("".join(t), ", ".join(tl))


def finnish(players):
    it = players.itervalues()
    pl = next(it)
    for p in it:
        if pl.ptype != p.ptype:
            return False
    return True


def parse_input(f):
    mp = []
    players = {}
    for j,l in enumerate(f):
        t = []
        for i,c in enumerate(l.strip()):
            t.append(c != '#')
            if c == 'G' or c == 'E':
                players[(i,j)] = PNJ(c, (i,j))
        mp.append(t)
    return mp, players


def jour15(f, limit=-1):
    # parse input
    mp, players = parse_input(f)
    print(players)
    print(sorted(players.values()))
    print("test:", len(players))
    print_map(mp, players)
    rnd = 0
    l = len(players)
    while not finnish(players) and rnd != limit:
        rnd += 1
        mv = False
        for p in sorted(players.values()):
            if finnish(players):
                rnd -= 1
                break
            if p not in players:
                continue
            players.pop(p)
            mv |= p.move(mp, players)
            p.attack(players)
            players[p] = p
            # print_map(mp, players)
        if mv or len(players) < l:
            l = len(players)
            print()
            print("round", rnd)
            print_map(mp, players)
    # print("round", rnd)
    # print_map(mp, players)
    print("Part 1:", rnd * sum(map(lambda x:x.hp, players.values())))


if __name__ == "__main__":
    input = "input15"
    if len(sys.argv) > 1:
        input = sys.argv[1]
    with open(input) as f:
        jour15(f)


# test
input1 = ["#######", "#.G...#", "#...EG#", "#.#.#G#", "#..G#E#", "#.....#", "#######"]
input2 = ["#######", "#G..#E#", "#E#E.E#", "#G.##.#", "#...#E#", "#...E.#", "#######"]
input3 = ["#######", "#E..EG#", "#.#G.E#", "#E.##E#", "#G..#.#", "#..E#.#", "#######"]
input4 = ["#######", "#E.G#.#", "#.#G..#", "#G.#.G#", "#G..#.#", "#...E.#", "#######"]
input5 = ["#######", "#.E...#", "#.#..G#", "#.###.#", "#E#G#G#", "#...#G#", "#######"]
input6 = ["#########", "#G......#", "#.E.#...#", "#..##..G#", "#...##..#", "#...#...#", "#.G...G.#", "#.....G.#", "#########"]

input_test = ["##########", "#...#...E#", "#...G....#", "#E..#....#", "##########"]

# jour15(input_test, 1)
# jour15(input6)
