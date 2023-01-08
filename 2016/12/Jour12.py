#!/usr/bin/python

import argparse
from Jour23 import regs

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 12')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        prog = [l.strip() for l in f]

    r = regs(prog)
    r.run()
    print("Part 1:", r.regs["a"])

    r = regs(prog, c=1)
    r.run()
    print("Part 2:", r.regs["a"])
