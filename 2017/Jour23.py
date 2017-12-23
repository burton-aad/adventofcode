#!/usr/bin/python

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
    with open("input23") as f:
        lines = f.readlines()
        # print(repr(lines))

    p = Prog23(0, lines)
    while p.is_running():
        p.run()

    print p.mul_cnt
