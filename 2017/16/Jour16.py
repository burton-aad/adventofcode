#!/usr/bin/python

import argparse


def spin(s, x):
    i = len(s) - int(x)
    return s[i:] + s[:i]

def exchange(s, c):
    a, b = map(int, c.split("/"))
    s[a], s[b] = s[b], s[a]
    return s

def partner(s, c):
    a, b = map(lambda x: s.index(x), c.split("/"))
    s[a], s[b] = s[b], s[a]
    return s

def dance(progs, commands):
    p = progs[:]
    # print(p)
    for c in commands:
        if c.startswith("s"):
            p = spin(p, c[1:])
        elif c.startswith("x"):
            p = exchange(p, c[1:])
        elif c.startswith("p"):
            p = partner(p, c[1:])
        else:
            pass
        # print(p)
    return p

pgs = list("abcdefghijklmnop")
number_of_dance = 1_000_000_000

def main(s):
    p = dance(pgs, s)
    print("Part 1:", "".join(p))
    i = 1

    # search loop
    while p != pgs:
        p = dance(p, s)
        i += 1
    # print(i, "->", "".join(p))

    p = pgs
    for _ in range(number_of_dance % i):
        p = dance(p, s)
    print("Part 2:", "".join(p))


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 16')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        s = f.read().strip().split(",")
    main(s)
