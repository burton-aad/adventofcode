#!/usr/bin/env python3

import sys
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
    with open(sys.argv[1]) as f:
        nums = [int(x) for x in f]
    nums.sort()
    l, r = find2(nums)
    print("part 1: {} {} -> {}".format(l, r, l * r))
    l, v, r = findn(nums, 3)
    print("part 2: {} {} {} -> {}".format(l, v, r, l * v * r))
