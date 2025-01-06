#!/usr/bin/env python

from __future__ import print_function
from enum import IntEnum


class Intcode(object):
    MAX_OP_PARAMS = 4

    class Opcode(IntEnum):
        Add = 1
        Mul = 2
        Input = 3
        Output = 4
        JumpTrue = 5
        JumpFalse = 6
        LessThan = 7
        Equals = 8
        End = 99

    def __init__(self, prog):
        self.reset(prog)
        self.op = {
            Intcode.Opcode.Add : (3, self.add),
            Intcode.Opcode.Mul : (3, self.mul),
            Intcode.Opcode.Input : (1, self.get_input),
            Intcode.Opcode.Output : (1, self.print_out),
            Intcode.Opcode.JumpTrue : (2, self.jump_if_true),
            Intcode.Opcode.JumpFalse : (2, self.jump_if_false),
            Intcode.Opcode.LessThan : (3, self.less_than),
            Intcode.Opcode.Equals : (3, self.equals),
            Intcode.Opcode.End : (0, self.end)
        }

    def reset(self, prog = None):
        if prog is not None:
            self.orig_prog = prog[:]
        self.prog = self.orig_prog[:]
        self.pc = 0
        self.capture_out = False
        self.out = []

    def init(self, noun, verb):
        self.prog[1] = noun
        self.prog[2] = verb

    def get_op(self, pc):
        p, op = divmod(self.prog[pc], 100)
        return [int(x) for x in reversed("0"*self.MAX_OP_PARAMS + str(p))], op

    def str_op(self, pc):
        params, opcode = self.get_op(pc)
        r = []
        if opcode in self.op:
            l, _ = self.op[opcode]
            for i, v in enumerate(self.prog[pc+1:pc+l+1]):
                r.append("{}{}".format("" if params[i] else "*", v))
            return "{} ({})".format(Intcode.Opcode(opcode).name, ", ".join(r))
        else:
            return str(self.prog[pc])

    def pretty_print(self):
        s = []
        pc = 0
        while pc < len(self.prog):
            opcode = self.prog[pc] % 100
            l, _ = self.op.get(opcode, (0, 0))
            s.append("{:02}: {}".format(pc, self.str_op(pc)))
            pc += l+1
        return "\n".join(s)

    def run(self, *input, capture_out=False):
        self.input = list(reversed(input))
        self.capture_out = capture_out
        while self.pc < len(self.prog):
            # print("prog: {}".format(self))
            params, opcode = self.get_op(self.pc)
            l, op = self.op[opcode]
            # print("op :", self.str_op(self.pc))
            op(params, *self.prog[self.pc+1:self.pc+l+1])
        if capture_out:
            return self.out

    def end(self, params):
        self.pc = len(self.prog) # this end the program

    def read_param(self, p, i):
        return i if p else self.prog[i]

    def add(self, params, in1, in2, out):
        self.prog[out] = self.read_param(params[0], in1) + self.read_param(params[1], in2)
        self.pc += 4

    def mul(self, params, in1, in2, out):
        self.prog[out] = self.read_param(params[0], in1) * self.read_param(params[1], in2)
        self.pc += 4

    def get_input(self, params, out):
        self.prog[out] = self.input.pop()
        self.pc += 2

    def print_out(self, params, in_):
        if self.capture_out:
            self.out.append(self.read_param(params[0], in_))
        else:
            print(self.read_param(params[0], in_))
        self.pc += 2

    def jump_if_true(self, params, in_, val):
        if self.read_param(params[0], in_):
            self.pc = self.read_param(params[1], val)
        else:
            self.pc += 3

    def jump_if_false(self, params, in_, val):
        if self.read_param(params[0], in_) == 0:
            self.pc = self.read_param(params[1], val)
        else:
            self.pc += 3

    def less_than(self, params, in1, in2, out):
        self.prog[out] = int(self.read_param(params[0], in1) < self.read_param(params[1], in2))
        self.pc += 4

    def equals(self, params, in1, in2, out):
        self.prog[out] = int(self.read_param(params[0], in1) == self.read_param(params[1], in2))
        self.pc += 4

    def __str__(self):
        return str([x if i != self.pc else "({})".format(x) for i, x in enumerate(self.prog)])
        # return str(self.prog)


