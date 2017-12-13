#!/usr/bin/python

from __future__ import print_function
import re

def print_v(*args):
    # print(*args)
    pass

class regs:
    def __init__(self, instructions, a=0, b=0, c=0, d=0):
        self.regs = {"a" : a, "b" : b, "c" : c, "d" : d}
        self.inst = list(map(lambda x: x.split(), instructions))
        # print(self.inst)

    def get(self, reg):
        if reg in self.regs:
            return self.regs[reg]
        else:
            return int(reg)

    def cpy(self, x, y):
        if y in self.regs:
            self.regs[y] = self.get(x)

    def inc(self, x):
        self.regs[x] += 1

    def dec(self, x):
        self.regs[x] -= 1

    def out(self, x):
        print(self.get(x))

    def jnz(self, x, y, p):
        if self.get(x) != 0:
            return self.get(y)
        else:
            return 1

    def tgl(self, x, p):
        p = p + self.get(x)
        if 0 <= p < len(self.inst):
            if self.inst[p][0] == "jnz":
                self.inst[p][0] = "cpy"
            elif self.inst[p][0] == "cpy":
                self.inst[p][0] = "jnz"
            elif self.inst[p][0] == "inc":
                self.inst[p][0] = "dec"
            else:
                self.inst[p][0] = "inc"

    def add_loop(self, m):
        if self.get(m.group(5)) == 0:
            return 0
        if m.group(5) == m.group(2):
            ng = 4
        else:
            ng = 2
        if m.group(ng-1) == "inc":
            self.regs[m.group(ng)] += abs(self.get(m.group(5)))
        else:
            self.regs[m.group(ng)] -= abs(self.get(m.group(5)))
        print_v("add_loop {} {}".format(m.group(ng), m.group(5)))
        return 3

    def mul_loop(self, m):
        reg = m.group(5) or m.group(11)
        op = m.group(4) or m.group(10)
        if op == "inc":
            self.regs[reg] += abs(self.get(m.group(1)) * self.get(m.group(13)))
        else:
            self.regs[reg] -= abs(self.get(m.group(1)) * self.get(m.group(13)))
        print_v("mul_loop {} {} {}".format(reg, m.group(1), m.group(13)))
        return 6

    def parse(self, l, p):
        if l[0] == "cpy":
            self.cpy(l[1], l[2])
        elif l[0] == "add":
            self.add(l[1], l[2])
        elif l[0] == "inc":
            self.inc(l[1])
        elif l[0] == "dec":
            self.dec(l[1])
        elif l[0] == "jnz":
            return self.jnz(l[1], l[2], p)
        elif l[0] == "out":
            self.out(l[1])
        elif l[0] == "tgl":
            self.tgl(l[1], p)
        return 1

    def __repr__(self):
        return str(self.regs)

    def run(self):
        p = 0
        f_limit = 6
        f = []
        sloop = re.compile(r"(inc|dec) (.)!(inc|dec) (.)!jnz (\2|\4) -2")
        dloop = re.compile(r"cpy (.) (.)!((inc|dec) (?!\1)(.)!(inc|dec) (\2)|(inc|dec) (\2)!(inc|dec) (?!\1)(.))!jnz \2 -2!(inc|dec) (.)!jnz \13 -5")
        while 0 <= p < len(self.inst):
            # print(self.regs)
            # print(self.inst[p])
            print_v(" ".join(self.inst[p]))
            f.append(" ".join(self.inst[p]))
            if len(f) > f_limit:
                f.pop(0)
            p += self.parse(self.inst[p], p)

            # optimise on the fly
            # double loop
            m = dloop.match("!".join(f))
            if m:
                p += self.mul_loop(m)
                continue

            # simple loop
            m = sloop.match("!".join(f[-3:]))
            if m :
                p += self.add_loop(m)
                continue


if __name__ == "__main__":
    with open("input23") as f:
        prog = map(lambda x: x.strip(), f.readlines())
        print(repr(prog))

    r = regs(prog, a=7)
    r.run()
    print(r)

    r = regs(prog, a=12)
    r.run()
    print(r)

