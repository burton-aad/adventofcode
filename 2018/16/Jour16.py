#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import re
import argparse
from pprint import pprint

def make_op(f):
    def op(regs, a, b, c):
        regs[c] = f(regs, a, b)
    return op

OPCODES = {
    "addr": make_op(lambda regs,a,b: regs[a] + regs[b]),
    "addi": make_op(lambda regs,a,b: regs[a] + b),

    "mulr": make_op(lambda regs,a,b: regs[a] * regs[b]),
    "muli": make_op(lambda regs,a,b: regs[a] * b),

    "banr": make_op(lambda regs,a,b: regs[a] & regs[b]),
    "bani": make_op(lambda regs,a,b: regs[a] & b),

    "borr": make_op(lambda regs,a,b: regs[a] | regs[b]),
    "bori": make_op(lambda regs,a,b: regs[a] | b),

    "setr": make_op(lambda regs,a,b: regs[a]),
    "seti": make_op(lambda regs,a,b: a),

    "gtir": make_op(lambda regs,a,b: int(a > regs[b])),
    "gtri": make_op(lambda regs,a,b: int(regs[a] > b)),
    "gtrr": make_op(lambda regs,a,b: int(regs[a] > regs[b])),

    "eqir": make_op(lambda regs,a,b: int(a == regs[b])),
    "eqri": make_op(lambda regs,a,b: int(regs[a] == b)),
    "eqrr": make_op(lambda regs,a,b: int(regs[a] == regs[b])),
}

def find_opcodes(sregs, op, waited):
    r = []
    for o in OPCODES:
        regs = list(sregs)
        OPCODES[o](regs, *op[1:])
        if regs == waited:
            r.append(o)
    return r


def parse_input(f):
    blank = 0
    samples = []
    prog = []
    b = re.compile(r"Before:\s*\[(\d), (\d), (\d), (\d)\]")
    a = re.compile(r"After:\s*\[(\d), (\d), (\d), (\d)\]")
    part = 1
    for l in f:
        if part == 1:
            # parse first part
            if len(l.strip()) == 0:
                blank += 1
                if blank > 2:
                    # Second part
                    part = 2
            else:
                blank = 0
                m = b.match(l)
                if m:
                    samples.append([[int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4))]])
                else:
                    m = a.match(l)
                    if m:
                        samples[-1].append([int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4))])
                    else:
                        samples[-1].append(list(map(int, l.split())))
        else:
            # parse second part
            prog.append(list(map(int, l.strip().split())))
    return samples, prog


def jour16(f):
    samples, prog = parse_input(f)
    count3 = 0
    possible_ops = [None] * len(OPCODES)
    for s in samples:
        ops_found = set(find_opcodes(*s))
        if len(ops_found) >= 3:
            count3 += 1
        op = s[1][0]
        if possible_ops[op] is None:
            possible_ops[op] = ops_found
        else:
            possible_ops[op] &= ops_found
    print("Part 1:", count3)

    # search opcode for each ops
    ops = [None] * len(OPCODES)
    while None in ops:
        i = list(map(len, possible_ops)).index(1)
        op = possible_ops[i].pop()
        ops[i] = OPCODES[op]
        for s in possible_ops:
            s -= set([op])
    # print("ops :", ops)

    # run program
    regs = [0]*4
    for i, inst in enumerate(prog):
        ops[inst[0]](regs, *inst[1:])
    print("Part 2:", regs[0])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 16')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        jour16(f)
