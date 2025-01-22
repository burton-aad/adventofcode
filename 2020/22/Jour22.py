#!/usr/bin/env python3

import sys
import argparse
from collections import deque
from itertools import islice

def score(p):
    return sum((i+1)*v for i,v in enumerate(reversed(p)))

def Part1(p1, p2):
    while len(p1) > 0 and len(p2) > 0:
        a, b = p1.popleft(), p2.popleft()
        if a > b:
            p1.extend([a, b])
        else:
            p2.extend([b, a])
    print("Part 1:", score(p1 if len(p1) > 0 else p2))

def Part2(p1, p2, i=1):
    infdog = set()
    while len(p1) > 0 and len(p2) > 0:
        h = (tuple(p1), tuple(p2))
        if h in infdog:
            # infinite loop p1 win
            return True
        infdog.add(h)
        a, b = p1.popleft(), p2.popleft()
        if a <= len(p1) and b <= len(p2):
            if Part2(deque(islice(p1, a)), deque(islice(p2, b)), i+1):
                p1.extend([a, b])
            else:
                p2.extend([b, a])
        elif a > b:
            p1.extend([a, b])
        else:
            p2.extend([b, a])
    if i == 1:
        print("Part 2:", score(p1 if len(p1) > 0 else p2))
    return len(p1) > 0


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 13')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    p1, p2 = [], []
    with open(args.input) as f:
        for l in f:
            l = l.strip()
            if l == "Player 1:":
                p = p1
            elif l == "Player 2:":
                p = p2
            elif l:
                p.append(int(l))
    Part1(deque(p1), deque(p2))
    Part2(deque(p1), deque(p2))
