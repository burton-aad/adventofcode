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


class Game:
    def __init__(self, carte, players):
        "players is the list of players"
        self.carte = carte
        self.players = {}
        for p in players:
            self.players[p] = p.copy()
        self.full_round = 0
        self.fini = False

    def extra_elfes_power(self, inc):
        for p in filter(lambda x: x.ptype == 'E', self.players.values()):
            p.ap += inc

    def finish(self):
        if not self.fini:
            it = self.players.itervalues()
            pl = next(it)
            self.fini = True
            for p in it:
                if pl.ptype != p.ptype:
                    self.fini = False
                    break
        return self.fini

    def print_map(self):
        for j,l in enumerate(self.carte):
            t = []
            tl = []
            for i,c in enumerate(l):
                if (i,j) in self.players:
                    t.append(self.players[(i,j)].ptype)
                    tl.append(self.players[(i,j)].plife())
                else:
                    t.append('.' if c else '#')
            print("".join(t), ", ".join(tl))

    def play_round(self):
        "return True if something change in the map (players have moved or died)"
        mv = False
        for p in sorted(self.players.values()):
            if self.finish():
                # current round does not end completely
                return mv
            if p.hp <= 0:
                # died from other attack
                continue
            self.players.pop(p)
            mv |= p.move(self.carte, self.players)
            self.players[p] = p
            mv |= p.attack(self.players)
        self.full_round += 1
        return mv



class PNJ:
    def __init__(self, ptype, pos, hp = 200, ap = 3):
        self.pos = pos
        self.ap = ap
        self.hp = hp
        self.ptype = ptype

    def copy(self):
        return PNJ(self.ptype, self.pos, self.hp, self.ap)

    def move(self, carte, players):
        "return True if the player move"
        q = [] # liste à faire
        qr = [] # prochaine liste

        # check if opponent in range
        for v in vois4(self.pos):
            if v in players:
                if players[v].ptype != self.ptype:
                    # can attack opponent, no move
                    return False
            elif carte[v[1]][v[0]]:
                q.append((v, v))

        # search accessible opponent
        # q is list of (first move, actual position)
        found = [] # will be filled with nearest targets
        r = set([self.pos]) # position already visited
        while not found and len(q) > 0:
            for p in q:
                move, cur = p
                r.add(cur)
                for v in vois4(cur):
                    if v in r:
                        pass
                    elif v in players:
                        if players[v].ptype != self.ptype:
                            found.append((move, cur))
                    elif carte[v[1]][v[0]]:
                        qr.append((move, v))
                        r.add(v)
            q = sorted(qr, cmp=readOrderCmp, key=lambda x: x[1])
            qr = []
        if found:
            found.sort(cmp=readOrderCmp, key=lambda x: x[1])
            # print(self, "move to", found[0][0])
            self.pos = found[0][0]
            return True
        return False

    def attack(self, players):
        "return True if an opponent die with this attack"
        p = sorted(filter(lambda x: x and x.ptype != self.ptype, map(players.get, vois4(self.pos))), key=lambda x: x.hp)
        # print(self, "attack", p)
        if len(p) > 0:
            p[0].hp -= self.ap
            if p[0].hp <= 0:
                # an ennemy died
                # print(p[0], "have died from", self)
                players.pop(p[0])
                return True
        return False

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



def parse_input(f):
    mp = []
    players = []
    for j,l in enumerate(f):
        t = []
        for i,c in enumerate(l.strip()):
            t.append(c != '#')
            if c == 'G' or c == 'E':
                players.append(PNJ(c, (i,j)))
        mp.append(t)
    return mp, players


def jour15(f, limit=-1, debug=False):
    # parse input
    mp, players = parse_input(f)

    # Part 1
    game = Game(mp, players)
    # print("players", len(players), ":", game.players)
    # game.print_map() # initial position
    while not game.finish() and game.full_round != limit:
        mv_or_die = game.play_round()
        if debug and mv_or_die:
            print("round", game.full_round)
            game.print_map()
    print("round", game.full_round)
    # game.print_map() # end position
    print("Part 1:", game.full_round * sum(map(lambda x:x.hp, game.players.values())))

    # Part 2
    num_elfes = len(filter(lambda x: x.ptype == 'E', players))
    inc_power = 0
    while True:
        inc_power += 1
        game = Game(mp, players)
        game.extra_elfes_power(inc_power)
        while not game.finish():
            mv_or_die = game.play_round()
            if debug and mv_or_die:
                print("round", game.full_round)
                game.print_map()
        n = len(filter(lambda x: x.ptype == 'E', game.players.values()))
        print("with {} power, {}/{} elfes stay alive".format(3+inc_power, n, num_elfes))
        if n == num_elfes:
            print("Part 2:", game.full_round * sum(map(lambda x:x.hp, game.players.values())))
            break



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
input_test2 = ["#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"]

# jour15(input_test2, 1)
# jour15(input6, debug=False)
