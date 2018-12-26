#!/usr/bin/python

from __future__ import print_function
import sys
if sys.version_info < (3,):
    range = xrange
from collections import deque

def jour09(players, last):
    c = deque([0])
    pts = [0] * players
    for i in range(1, last+1):
        if i % 23 == 0:
            c.rotate(7)
            p = (i-1) % players
            pts[p] += i + c.popleft()
        else:
            c.rotate(-2)
            c.appendleft(i)
        # print(c)
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
