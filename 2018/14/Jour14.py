#!/usr/bin/python

from __future__ import print_function
import sys
import argparse


def jour14(end):
    r = [3, 7]
    e1 = 0
    e2 = 1
    while len(r) < end+10:
        nr = r[e1] + r[e2]
        if nr > 9:
            r.append(nr//10)
            r.append(nr%10)
        else:
            r.append(nr)
        e1 = (e1 + r[e1] + 1) % len(r)
        e2 = (e2 + r[e2] + 1) % len(r)
    print("Part 1:", "".join(map(str, r[end:end+10])))


def jour14_2(end):
    ends = str(end)
    endl = len(ends)
    r = [3, 7]
    e1 = 0
    e2 = 1
    while "".join(map(str, r[-endl:])) != ends and "".join(map(str, r[-endl-1:-1])) != ends:
        # print(r)
        nr = r[e1] + r[e2]
        if nr > 9:
            r.append(nr//10)
            r.append(nr%10)
        else:
            r.append(nr)
        e1 = (e1 + r[e1] + 1) % len(r)
        e2 = (e2 + r[e2] + 1) % len(r)
    if "".join(map(str, r[-endl:])) == ends:
        print("Part 2:", len(r)-endl)
    else:
        print("Part 2:", len(r)-endl-1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 14')
    parser.add_argument("recipes", nargs='?', type=int, default=894501)
    args = parser.parse_args()

    jour14(args.recipes)
    jour14_2(args.recipes)
