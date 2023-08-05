#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, os
import argparse
import re
import copy

sys.path.append(os.path.join(os.path.dirname(__file__), "..", "16"))
from Jour16 import OPCODES


class CPU:
    def __init__(self, prog, ip_reg):
        self.regs = [0]*6
        self.ip = 0
        self.ip_reg = ip_reg
        self.prog = copy.deepcopy(prog)

    def run(self):
        while 0 <= self.ip < len(self.prog):
            self.run_instruction()

    def run_instruction(self):
        self.regs[self.ip_reg] = self.ip
        # print("ip={} {}".format(self.ip, self.regs), end=" ")
        op, a, b, c = self.prog[self.ip]
        # print("{} {} {} {}".format(op, a, b, c), end=" ")
        OPCODES[op](self.regs, a, b, c)
        # print(self.regs)
        self.ip = self.regs[self.ip_reg]
        self.ip += 1

def parse_input(f):
    r = re.compile(r"(.*) (\d+) (\d+) (\d+)")
    prog = []
    for l in f:
        if l.startswith("#"):
            ip = int(l.split()[-1])
        else:
            m = r.match(l)
            prog.append([m.group(1), int(m.group(2)), int(m.group(3)), int(m.group(4))])
    return ip, prog

def jour19(f):
    ip_reg, prog = parse_input(f)
    print(ip_reg)
    print(prog)

    cpu = CPU(prog, ip_reg)
    cpu.run()
    print("Part 1:", cpu.regs[0])

    # cpu = CPU(prog, ip_reg)
    # cpu.regs[0] = 1
    # cpu.run()
    # print("Part 2:", cpu.regs[0])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 19')
    parser.add_argument('input_file', nargs='?', default="input", help='the input file')
    args = parser.parse_args(sys.argv[1:])
    with open(args.input_file) as f:
        jour19(f)


#test
# jour19(["#ip 0", "seti 5 0 1", "seti 6 0 2", "addi 0 1 0", "addr 1 2 3", "setr 1 0 0", "seti 8 0 4", "seti 9 0 5"])
