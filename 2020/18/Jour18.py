#!/usr/bin/env python3

import sys
import re
import argparse

ops = {
    None: lambda x, y: y,
    '+' : lambda x, y: x + y,
    '*' : lambda x, y: x * y,
}

def parse_rec(it):
    r = 0
    op = None
    for x in it:
        if '0' <= x <= '9':
            r = ops[op](r, int(x))
        elif x == '(':
            r = ops[op](r, parse_rec(it))
        elif x == ')':
            return r
        else:
            op = x
    return r

def parse(l):
    it = iter(re.findall(r"\S", l))
    return parse_rec(it)

class Num:
    def __init__(self, val):
        self.val = val

    def __add__(self, o):
        return Num(self.val * o.val)

    def __mul__(self, o):
        return Num(self.val + o.val)

    def __repr__(self):
        return "Num({})".format(self.val)

def part2(l):
    # methode du pauvre XD
    a = Num(0)
    b = Num(1)
    c = Num(2)
    d = Num(3)
    e = Num(4)
    f = Num(5)
    g = Num(6)
    h = Num(7)
    i = Num(8)
    j = Num(9)
    l = l.translate(str.maketrans("0123456789+*", "abcdefghij*+"))
    return eval(l).val

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 18')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        opers = [l.strip() for l in f]

    print("Part 1:", sum(parse(o) for o in opers))
    print("Part 2:", sum(part2(o) for o in opers))
