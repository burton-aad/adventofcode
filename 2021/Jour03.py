#!/usr/bin/env python3

import sys, os
from collections import Counter
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from utils import partition
import time

def loop_filter(l, comp):
    rank = 0
    while len(l) > 1:
        o, z = map(list, partition(lambda x: x[rank] == '1', l))
        l = o if comp(o, z) else z
        rank += 1
    return l[0]


def main(infile):
    with open(infile) as f:
        vals = [l.strip() for l in f]

    gamma, epsilon = [int("".join(t), 2) for t in zip(*[dict(Counter(l).most_common())
                                                        for l in zip(*vals)])]
    print("Part 1:", gamma * epsilon)

    ox = int(loop_filter(vals, lambda o, z: len(o) >= len(z)), 2)
    co2 = int(loop_filter(vals, lambda o, z: len(o) < len(z)), 2)
    print("Part 2:", ox * co2)

if __name__=="__main__":
    main(sys.argv[1])
