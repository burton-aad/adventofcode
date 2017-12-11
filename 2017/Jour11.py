#!/usr/bin/python

def add_tab_v(d, v, tab):
    if d[tab[0]] > 0:
        d[tab[0]] -= 1
        return
    for i,j in tab[1:]:
        if d[i] > 0:
            d[i] -= 1
            d[j] += 1
            return
    d[v] += 1

funcs = {
    "n"  : lambda d: add_tab_v(d, "n",  ["s",  ("se", "ne"), ("sw", "nw")]),
    "ne" : lambda d: add_tab_v(d, "ne", ["sw", ("nw", "n"),  ("s", "se")]),
    "se" : lambda d: add_tab_v(d, "se", ["nw", ("sw", "s"), ("n", "ne")]),
    "s"  : lambda d: add_tab_v(d, "s",  ["n",  ("ne", "se"), ("nw", "sw")]),
    "sw" : lambda d: add_tab_v(d, "sw", ["ne", ("n", "nw"),  ("se", "s")]),
    "nw" : lambda d: add_tab_v(d, "nw", ["se", ("s", "sw"), ("ne", "n")])
}


d = {"n" : 0, "ne" : 0, "se" : 0, "s" : 0, "sw" : 0, "nw" : 0}

m = 0
with open("input11") as f:
    s = f.read().strip().split(",")
    for i in s:
        funcs[i](d)
        sm = sum(d.values())
        if sm > m:
            m = sm

print d
print sum(d.values())
print m

