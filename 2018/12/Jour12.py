#!/usr/bin/python

from __future__ import print_function
import sys
import collections
import argparse


def potsToNum(pots):
    out = 0
    for p in pots:
        out = (out << 1) | (p == '#')
    return out

class Garden:
    def __init__(self, init_state):
        self.pots = collections.deque([x == "#" for x in init_state])
        self.idx = 0
        self.rules = [False]*32
        self.time = 0

    def add_rule(self, rule):
        key, _, val = rule.split()
        self.rules[potsToNum(key)] = val == "#"

    def __str__(self):
        return "".join(map(lambda x: "#" if x else ".", self.pots))

    def sum_num(self):
        return sum([i+self.idx for i,p in enumerate(self.pots) if p])

    def process(self, time=1):
        i = 0
        while i < time:
            i += 1
            c = 0
            l = len(self.pots)
            r = list(self.pots)
            ri = self.idx
            for _ in range(l):
                c = ((c << 1) | self.pots.popleft()) & 0x1f
                self.pots.append(self.rules[c])
            for _ in range(4):
                c = (c << 1) & 0x1f
                self.pots.append(self.rules[c])
            self.idx -= 2
            while not self.pots[0]:
                self.pots.popleft()
                self.idx += 1
            while not self.pots[-1]:
                self.pots.pop()
            if r == list(self.pots):
                self.idx += (self.idx - ri) * (time - i)
                return

def jour12(garden):
    for i in range(20):
        # print(i, ":", garden, garden.idx)
        garden.process()
    print("Part 1:", garden.sum_num())

    garden.process(50_000_000_000 - 20)
    print("Part 2:", garden.sum_num())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 12')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        init_state = f.readline().split()[2]
        print(init_state)
        g = Garden(init_state)
        f.readline() # empty line
        for l in f:
            g.add_rule(l)
    jour12(g)
