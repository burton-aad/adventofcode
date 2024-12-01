#!/usr/bin/env python3

import sys
import re
from itertools import product

mask_re = re.compile(r"mask = (.*)")
mem_re = re.compile(r"mem\[(\d+)\] = (\d+)")

def v1(prog):
    mask_0 = 0
    mask_1 = 0xFFFFFFFFF
    mask = lambda v: (v & mask_1) + (mask_0 ^ (mask_0 & v))
    mem = {}

    for l in prog:
        if m := mask_re.match(l):
            mask_0 = int(m.group(1).replace("X", "0"), 2)
            mask_1 = int(m.group(1).replace("X", "1"), 2)
        elif m := mem_re.match(l):
            mem[m.group(1)] = mask(int(m.group(2)))

    print("Part 1:", sum(mem.values()))

def v2(prog):
    mask_list = [0]
    mask = lambda v: [mask_list[0] | v ^ m | ((v & m) ^ m) for m in mask_list]
    mem = {}

    for l in prog:
        if m := mask_re.match(l):
            c = m.group(1).count("X")
            m = m.group(1).replace("X", r"{}")
            mask_list = [int(m.format(*v), 2) for v in product(range(2), repeat=c)]
        elif m := mem_re.match(l):
            for addr in mask(int(m.group(1))):
                mem[addr] = int(m.group(2))
    print("Part 2:", sum(mem.values()))

if __name__=="__main__":
    with open(sys.argv[1]) as f:
        prog = [l.strip() for l in f]
    v1(prog)
    v2(prog)
