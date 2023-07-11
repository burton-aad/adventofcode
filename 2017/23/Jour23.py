#!/usr/bin/python

import sys, os
import argparse

sys.path.append(os.path.join(os.path.dirname(__file__), "..", "18"))
from Jour18 import Prog

class Prog23(Prog):
    def __init__(self, pid, code=None):
        Prog.__init__(self, pid, code)
        self.mul_cnt = 0

    def sub(self, x, y):
        self.regs[x] = self.get(x) - self.get(y)

    def jnz(self, x, y):
        if self.get(x) != 0:
            return self.get(y)

    def mul(self, x, y):
        self.mul_cnt += 1
        Prog.mul(self, x, y)



if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 23')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        lines = f.readlines()

    p = Prog23(0, lines)
    while p.is_running():
        p.run()
    print("Part 1:", p.mul_cnt)
