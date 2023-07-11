#!/usr/bin/python

import argparse


def spin_run(inp, n):
    t = [0]
    p = 0
    for s in range(1, n+1):
        p = ((p + inp) % len(t)) + 1
        t.insert(p, s)
        # print t
    return t

def spin_run_low(inp, n):
    p = 0
    t = 1
    r = 1
    for s in range(1, n+1):
        p = ((p + inp) % t) + 1
        if p == 1:
            r = s
            # print s
        t+=1
    return r


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 17')
    parser.add_argument("steps", nargs='?', type=int, default=312)
    args = parser.parse_args()

    t = spin_run(args.steps, 2017)
    print("Part 1:", t[t.index(2017)+1])

    print("Part 2:", spin_run_low(args.steps, 50000000))
