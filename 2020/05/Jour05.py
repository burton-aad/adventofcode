#!/usr/bin/env python3

import sys
import argparse

def seat_id(s):
    return int(s.translate(s.maketrans("FBLR", "0101")), 2)

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 05')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        l = sorted([seat_id(l.strip()) for l in f])
    print("Part 1 : {}".format(max(l)))
    r = [i for x,y in zip(l, l[1:]) for i in range(x+1, y) if y-x > 1]
    print("Part 2 : {}".format(r[0]))

