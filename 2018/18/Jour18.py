#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import argparse

if sys.version_info < (3,):
    range = xrange

OPEN = '.'
TREE = '|'
LUMBERYARD = '#'
EXTRA = '?'

def vois8(i,j):
    return [(i-1,j), (i-1,j-1), (i,j-1), (i+1,j-1), (i+1,j), (i+1,j+1), (i,j+1), (i-1,j+1)]

class GameOfLife:
    def __init__(self, f):
        self.states = []
        for l in f:
            self.states.append([EXTRA] + list(l.strip()) + [EXTRA])
        self.h = len(self.states) + 2
        self.w = len(self.states[0])
        self.states = [[EXTRA] * self.w] + self.states + [[EXTRA] * self.w]

    def print(self):
        for i in range(1, self.h-1):
            print("".join(self.states[i][1:self.w-1]))

    def play_round(self, end=1):
        s = {}
        extra = 0
        for i in range(end):
            self.states = self.__round()
            rs = "".join(map("".join, self.states))
            if rs in s:
                # looping states
                print("state loop between minutes {} and {}".format(s[rs], i))
                extra = (end - i) % (i - s[rs])
                # print("test: end {}, i {}, loop {}, extra {}".format(end, i, i - s[rs], extra))
                break
            s[rs] = i
        for _ in range(extra-1):
            self.states = self.__round()

    def __round(self):
        r = []
        for j,l in enumerate(self.states):
            rl = []
            for i,c in enumerate(l):
                # t = list(map(lambda y,x: self.states[x][y], vois8(i,j)))
                if c == OPEN:
                    t = [self.states[x][y] for y,x in vois8(i,j)]
                    if t.count(TREE) >= 3:
                        rl.append(TREE)
                    else:
                        rl.append(OPEN)
                elif c == TREE:
                    t = [self.states[x][y] for y,x in vois8(i,j)]
                    if t.count(LUMBERYARD) >= 3:
                        rl.append(LUMBERYARD)
                    else:
                        rl.append(TREE)
                elif c == LUMBERYARD:
                    t = [self.states[x][y] for y,x in vois8(i,j)]
                    if t.count(LUMBERYARD) > 0 and t.count(TREE) > 0:
                        rl.append(LUMBERYARD)
                    else:
                        rl.append(OPEN)
                else:
                    rl.append(EXTRA)
            r.append(rl)
        return r

    def resources(self):
        tree = 0
        lumb = 0
        for l in self.states:
            tree += l.count(TREE)
            lumb += l.count(LUMBERYARD)
        return tree*lumb


def jour18(f):
    g = GameOfLife(f)
    g.print()
    for _ in range(10):
        g.play_round()
        print("-"*30)
        g.print()
    print("Part 1:", g.resources())

    p2 = 1000000000
    g.play_round(p2 - 10)
    # g.print()
    print("Part 2:", g.resources())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 18')
    parser.add_argument('input_file', nargs='?', default="input", help='the input file')
    args = parser.parse_args(sys.argv[1:])
    with open(args.input_file) as f:
        jour18(f)

# test
# jour18([".#.#...|#.", ".....#|##|", ".|..|...#.", "..|#.....#", "#.#|||#|#|", "...#.||...", ".|....|...", "||...#|.#|", "|.||||..|.", "...#.|..|."])
