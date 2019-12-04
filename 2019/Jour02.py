#!/usr/bin/env python

from __future__ import print_function
import sys
from enum import IntEnum
import itertools

class Intcode(object):
    class Opcode(IntEnum):
        Add = 1
        Mul = 2
        End = 99

    def __init__(self, prog):
        self.reset(prog)
        self.op = {
            Intcode.Opcode.Add : self.add,
            Intcode.Opcode.Mul : self.mul,
            Intcode.Opcode.End : self.end
        }

    def reset(self, prog):
        self.prog = prog[:]
        self.pc = 0

    def init(self, noun, verb):
        self.prog[1] = noun
        self.prog[2] = verb

    def run(self):
        while self.pc < len(self.prog):
            self.pc += self.op[self.prog[self.pc]](*self.prog[self.pc+1:])

    def add(self, *args):
        self.prog[args[2]] = self.prog[args[1]] + self.prog[args[0]]
        return 4

    def mul(self, *args):
        self.prog[args[2]] = self.prog[args[1]] * self.prog[args[0]]
        return 4

    def end(self, *args):
        return len(self.prog) # this end the program

    def __str__(self):
        return str(self.prog)


def main(v):
    p = Intcode(v)
    p.init(12, 2)
    p.run()
    # print(p)
    print("part 1 :", p.prog[0])

    # seek for specific value
    target = 19690720
    for i, j in itertools.product(range(100), repeat=2):
        p.reset(v)
        p.init(i, j)
        p.run()
        if p.prog[0] == target:
            print("part 2 : {} (100 * {} + {})".format(100*i+j, i, j))


# main([1,9,10,3,2,3,11,0,99,30,40,50])
# main([2,4,4,5,99,0])
# main([1,1,1,4,99,5,6,0,99])

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage : {} <input>".format(sys.argv[0]))
        sys.exit(1)
    with open(sys.argv[1]) as f:
        v = [int(x) for x in f.read().strip().split(',')]
        main(v)
