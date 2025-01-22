#!/usr/bin/env python3

import sys
import argparse

class Ops:
    def __init__(self):
        self._acc = 0
        self.pc = 0

    def nop(self, x):
        self.pc += 1

    def acc(self, x):
        self._acc += x
        self.pc += 1

    def jmp(self, x):
        self.pc += x

    def __str__(self):
        return "ops(acc {}, pc {})".format(self._acc, self.pc)

def run(prog):
    ops = Ops()
    s = set()
    while ops.pc < len(prog) and ops.pc not in s:
        s.add(ops.pc)
        op, x = prog[ops.pc]
        getattr(ops, op)(x)
    return ops

def Jour08(prog):
    ops = run(prog)
    print("Part 1:", ops._acc)

    t = {"nop": "jmp", "jmp": "nop"}
    for i, (ins, _) in enumerate(prog):
        if ins not in t:
            continue
        prog[i][0] = t[ins]
        ops = run(prog)
        prog[i][0] = ins
        if ops.pc >= len(prog):
            print("Part 2:", ops._acc)
            break
        


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 08')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()
    with open(args.input) as f:
        prog = [[i, int(x)] for i, x in iter(lambda: f.readline().split(), [])]
    Jour08(prog)
