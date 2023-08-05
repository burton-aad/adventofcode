#!/usr/bin/python

from __future__ import print_function
import sys
if sys.version_info < (3,):
    range = xrange

class linkedVec:
    def __init__(self, init_vec=None, limit=100000):
        self.v = []
        self.limit = limit
        if init_vec is not None:
            self.v.append(init_vec[:])
            self.size = len(init_vec)
        else:
            self.v.append([])
            self.size = 0

    def reorg(self, i):
        l = len(self.v[i])
        if l > self.limit:
            self.v.insert(i+1, self.v[i][l//2:])
            self.v[i][l//2:] = []

    def insert(self, idx, item):
        i = 0
        while idx > len(self.v[i]):
            idx -= len(self.v[i])
            i += 1
        self.v[i].insert(idx, item)
        self.size += 1
        self.reorg(i)

    def pop(self, idx):
        i = 0
        while idx >= len(self.v[i]):
            idx -= len(self.v[i])
            i += 1
        self.size -= 1
        return self.v[i].pop(idx)

    def __len__(self):
        return self.size

    def format(self):
        return "{}".format(self.v)

def jour09(players, last):
    c = linkedVec([0])
    idx = 0
    pts = [0] * players
    step  = last / 10
    s = 0
    for i in range(1, last+1):
        if i % step == 1:
            print("{}%...".format(s), end='')
            s += 10
        if i % 23 == 0:
            idx = (idx - 7) % len(c)
            p = (i-1) % players
            pts[p] += i + c.pop(idx)
        else:
            idx = ((idx + 1) % len(c)) + 1
            c.insert(idx, i)
        # print(c.format())
    print()
    return pts

# print(jour09(9, 25))
# print(max(jour09(21, 6111)))
# print(max(jour09(13, 7999)))

# input : 458 players; last marble is worth 72019 points
if __name__ == "__main__":
    players = 458
    last = 72019
    if len(sys.argv) > 1:
        players = int(sys.argv[1])
    if len(sys.argv) > 2:
        last = int(sys.argv[2])

    print("Part 1:", max(jour09(players, last)))
    print("Part 2:", max(jour09(players, 100*last)))
