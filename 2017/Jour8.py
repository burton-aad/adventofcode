#!/usr/bin/python


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

m = 0
with open("input8") as f:
    r = {}
    for l in f:
        v = run_line(r, l)
        if v > m:
            m = v
    print r
    print max(r.iteritems(), key=lambda x: x[1])
    print m
