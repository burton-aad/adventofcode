#!/usr/bin/env python

# input : 272091-815432

import itertools

def sorted_num(n):
    return list(str(n)) == sorted(str(n))

def have2Adjacent(n):
    return any([len(list(g)) > 1 for _, g in itertools.groupby(str(n))])

def haveExact2Adjacent(n):
    return any([len(list(g)) == 2 for _, g in itertools.groupby(str(n))])

def main():
    r = []
    r2 = []
    for i in range(272091, 815432):
        if sorted_num(i):
            if haveExact2Adjacent(i):
                r.append(i)
                r2.append(i)
            elif have2Adjacent(i):
                r.append(i)
    print("Part 1:", len(r))
    print("Part 2:", len(r2))


