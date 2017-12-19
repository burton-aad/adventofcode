#!/usr/bin/python

from __future__ import print_function
import re

def print_v(*args):
    # print(*args)
    pass

class Prog:
    valid_keys = map(lambda x: chr(ord('a')+x), range(26))
    STATE_RUN = 0
    STATE_STUCK = 1
    STATE_END = 2

    def __init__(self, pid, code=None):
        self.recv_cnt = 0
        self.snd_cnt = 0
        self.state = Prog.STATE_RUN
        self.pc = 0
        self.pid = pid
        self.pipe = []
        self.regs = {'p': self.pid}
        self.mirror_prog = self
        if code:
            self.load(code)
        else:
            self.inst = None

    def is_running(self):
        return self.state == Prog.STATE_RUN

    def load(self, code):
        self.inst = map(lambda x: x.strip().split(), code)
        self.pc = 0

    def set_mirror(self, prg):
        self.mirror_prog = prg
        prg.mirror_prog = self

    def get(self, reg):
        if reg in self.valid_keys:
            return self.regs.get(reg, 0)
        else:
            return int(reg)

    def __repr__(self):
        return "P{}: pipe {}, regs{}".format(self.pid, self.pipe, self.regs)

    def set(self, x, y):
        self.regs[x] = self.get(y)

    def add(self, x, y):
        self.regs[x] = self.get(x) + self.get(y)

    def mul(self, x, y):
        self.regs[x] = self.get(x) * self.get(y)

    def mod(self, x, y):
        self.regs[x] = self.regs[x] % self.get(y)

    def rcv_mirror_val(self, x):
        self.pipe.append(x)
        if self.state == Prog.STATE_STUCK:
            self.state = Prog.STATE_RUN

    def snd(self, x):
        self.mirror_prog.rcv_mirror_val(self.get(x))
        self.snd_cnt += 1

    def rcv(self, x):
        if len(self.pipe) == 0:
            self.state = Prog.STATE_STUCK
            return 0
        if self.recv_cnt == 0:
            print("Case 1: Prog {} recv {}".format(self.pid, self.pipe[-1]))
        self.regs[x] = self.pipe.pop(0)
        self.recv_cnt += 1

    def jgz(self, x, y):
        if self.get(x) > 0:
            return self.get(y)

    def parse(self, l):
        r = getattr(self, l[0])(*l[1:])
        return r if r is not None else 1

    def run(self, num_inst=1):
        p = self.pc
        while self.is_running() and num_inst > 0:
            print_v("{}: {}".format(self.pid, " ".join(self.inst[p])))
            p += self.parse(self.inst[p])
            if not (0 <= p < len(self.inst)):
                print_v("End prog {}". format(self.pid))
                self.state = Prog.STATE_END
            num_inst -= 1
        self.pc = p
        print_v(self)

if __name__ == "__main__":
    with open("input18") as f:
        lines = f.readlines()
        # print(repr(lines))

    p = Prog(0, lines)
    while p.recv_cnt == 0 and p.is_running():
        p.run()

    print()

    p0 = Prog(0, lines)
    p1 = Prog(1, lines)
    p0.set_mirror(p1)
    while True:
        p0.run()
        p1.run()
        if (not p0.is_running()) and (not p1.is_running()):
            # run one more to see if it is correctly ending
            p0.run()
            p1.run()
            if (not p0.is_running()) and (not p1.is_running()):
                break
    print("Case 2: prog 1 send count {}".format(p1.snd_cnt))
    print_v(p0)
    print_v(p1)
