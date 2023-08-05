#!/usr/bin/python

from __future__ import print_function
import sys
import argparse

def man_dist(p1, p2):
    return abs(p1[0]-p2[0]) + abs(p1[1]-p2[1])


def jour06(pts):
    x_max = max([p[0] for p in pts])
    y_max = max([p[1] for p in pts])
    print("grid :", x_max, y_max)

    p1_sizes = [0] * len(pts)
    p2_size = 0
    lim_p2 = 10000
    for x in range(x_max+1):
        for y in range(y_max+1):
            t = list(map(lambda p:man_dist(p, (x,y)), pts))

            m = min(t)
            i = t.index(m)
            if t.count(m) == 1 and p1_sizes[i] is not None:
                p1_sizes[i] += 1
                if x == 0 or x == x_max or y == 0 or y == y_max:
                    # infinite
                    p1_sizes[i] = None

            s = sum(t)
            if s < lim_p2 :
                p2_size += 1

    # print(p1_sizes)
    print("part 1 :", max(s for s in p1_sizes if s is not None))
    print("part 2 :", p2_size)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 06')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    p = []
    with open(args.input) as f:
        for l in f:
            i,j = l.split(", ")
            p.append((int(i), int(j)))
    # print(p)
    jour06(p)
