#!/usr/bin/python

import argparse

def jump1(_):
    return 1

def jump2(v):
    return 1 if v < 3 else -1

def run_until_exit(t, jump_func):
    # run to the end
    s = 0
    p = 0
    while 0 <= p < len(t):
        o = p
        p += t[p]
        t[o] += jump_func(t[o])
        s += 1
    return s


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 05')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        t = [int(l.strip()) for l in f]

    print("Part 1: ", run_until_exit(t.copy(), jump1))
    print("Part 2: ", run_until_exit(t, jump2))
