#!/usr/bin/python

import argparse

fcomp = {
    "==" : lambda x,y: x == y,
    "!=" : lambda x,y: x != y,
    ">"  : lambda x,y: x > y,
    ">=" : lambda x,y: x >= y,
    "<"  : lambda x,y: x < y,
    "<=" : lambda x,y: x <= y,
}

def run_line(regs, l):
    reg, op, val, _, r, comp, v2 = l.split()
    if fcomp[comp](regs.get(r, 0), int(v2)) :
        if op == "inc":
            regs[reg] = regs.get(reg, 0) + int(val)
        else:
            regs[reg] = regs.get(reg, 0) - int(val)
    return regs.get(reg, 0)


def run_program(pg_path):
    r = {}
    with open(pg_path) as f:
        m = max(run_line(r, l) for l in f)
    return r, m

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 08')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    regs, m = run_program(args.input)
    # print(regs)
    print("Part 1:", max(regs.values()))
    print("Part 2:", m)
