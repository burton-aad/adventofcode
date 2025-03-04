#!/usr/bin/env python3

import sys
import argparse
from itertools import combinations, count
from bisect import bisect

def find2(nums):
    # O(n)
    l = enumerate(nums)
    li, lv = next(l)
    r = zip(count(len(nums)-1, -1), reversed(nums))
    ri, rv = next(r)
    while li < ri:
        n = lv + rv
        if n < 2020:
            li, lv = next(l)
        elif n > 2020:
            ri, rv = next(r)
        else:
            break
    return lv, rv

def findn(nums, order):
    # O(n^order)
    for values in combinations(nums, order):
        if sum(values) == 2020:
            return values

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 01')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        nums = [int(x) for x in f]
    nums.sort()
    l, r = find2(nums)
    print("{} {} -> {}".format(l, r, l * r))
    print("Part 1: {}".format(l * r))
    l, v, r = findn(nums, 3)
    print("{} {} {} -> {}".format(l, v, r, l * v * r))
    print("Part 2: {}".format(l * v * r))
