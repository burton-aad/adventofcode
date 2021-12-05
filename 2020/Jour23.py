#!/usr/bin/env python3

import sys
from collections import deque

def target(d, l, cur, max_cup):
    t = cur - 1
    if t == 0:
        t = max_cup
    while t in l:
        t -= 1
        if t == 0:
            t = max_cup
    return t

def move(d, m = 9):
    cur = d[0]
    d.rotate(-1)
    l = [d.popleft() for _ in range(3)]
    dest = target(d, l, cur, m)
    d.rotate(-d.index(dest)-1)
    d.extend(l)
    d.rotate(-d.index(cur)-1)


def Jour23(p):
    p = deque(int(i) for i in str(p))
    for _ in range(1000):
        move(p)
    p.rotate(-p.index(1))
    p.popleft()
    print("".join(map(str, p)))

if __name__=="__main__":
    # input = 389125467
    input = 562893147
    Jour23(input)
