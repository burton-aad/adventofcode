#!/usr/bin/env python3

import sys
from collections import deque
from itertools import combinations

def valid(i, q):
    return any(i == x + y for x,y in combinations(q, 2))

def Jour09(transmit, preambule=25):
    q = deque(transmit[:preambule])
    for i in transmit[preambule:]:
        if not valid(i, q):
            print("Part 1:", i)
            break
        q.append(i)
        q.popleft()

    invalid = i
    i, j = 0,2
    while (s := sum(transmit[i:j])) != invalid:
        if s < invalid:
            j += 1
        else:
            i += 1
    print("Part 2:", min(transmit[i:j]) + max(transmit[i:j]))


if __name__=="__main__":
    with open(sys.argv[1]) as f:
        transmit = [int(l) for l in f]
    Jour09(transmit)
