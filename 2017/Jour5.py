#!/usr/bin/python


def run_until_exit(t):
    # run to the end
    s = 0
    p = 0
    while 0 <= p < len(t):
        o = p
        p += t[p]
        if t[o] >= 3:
            t[o] -= 1
        else:
            t[o] += 1
        s += 1
    return t, s

t = []

with open("input5") as f:
    for l in f:
        t.append(int(l.strip()))

print run_until_exit(t)
