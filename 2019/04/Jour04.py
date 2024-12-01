#!/usr/bin/env python

# input : 272091-815432

import argparse
import itertools

def sorted_num(n):
    return list(str(n)) == sorted(str(n))

def have2Adjacent(n):
    return any([len(list(g)) > 1 for _, g in itertools.groupby(str(n))])

def haveExact2Adjacent(n):
    return any([len(list(g)) == 2 for _, g in itertools.groupby(str(n))])

def main(start, end):
    r = []
    r2 = []
    for i in range(start, end):
        if sorted_num(i):
            if haveExact2Adjacent(i):
                r.append(i)
                r2.append(i)
            elif have2Adjacent(i):
                r.append(i)
    print("Part 1:", len(r))
    print("Part 2:", len(r2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2019 - Jour 04')
    parser.add_argument("start", nargs='?', type=int, default=272091)
    parser.add_argument("end", nargs='?', type=int, default=815432)
    args = parser.parse_args()

    main(args.start, args.end)
