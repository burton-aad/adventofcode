#!/usr/bin/python

import argparse
from Jour23 import regs

parser = argparse.ArgumentParser(description='AoC 2016 - Jour 25')
parser.add_argument("input", nargs='?', help="input file", default="input")
parser.add_argument("-a", type=int, help="initial value of a", default=0)
args = parser.parse_args()

with open(args.input) as f:
    prog = map(lambda x: x.strip(), f.readlines())

r = regs(prog, a=args.a)
r.run()
