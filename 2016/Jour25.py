#!/usr/bin/python

import sys
from Jour23 import regs

with open("input25") as f:
    prog = map(lambda x: x.strip(), f.readlines())

    a = 0
    if len(sys.argv) > 1:
        a = int(sys.argv[1])
    r = regs(prog, a=a)
    r.run()
