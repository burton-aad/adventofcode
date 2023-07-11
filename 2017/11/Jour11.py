#!/usr/bin/python

import argparse


def add_tab_v(d, v, r, tab):
    if d[r] > 0:
        d[r] -= 1
    else:
        for i,j in tab:
            if d[i] > 0:
                d[i] -= 1
                d[j] += 1
                break
        else:
            d[v] += 1
    # return current distance from start
    return sum(d.values())


funcs = {
    "n"  : lambda d: add_tab_v(d, "n",  "s",  [("se", "ne"), ("sw", "nw")]),
    "ne" : lambda d: add_tab_v(d, "ne", "sw", [("nw", "n"),  ("s", "se")]),
    "se" : lambda d: add_tab_v(d, "se", "nw", [("sw", "s"), ("n", "ne")]),
    "s"  : lambda d: add_tab_v(d, "s",  "n",  [("ne", "se"), ("nw", "sw")]),
    "sw" : lambda d: add_tab_v(d, "sw", "ne", [("n", "nw"),  ("se", "s")]),
    "nw" : lambda d: add_tab_v(d, "nw", "se", [("s", "sw"), ("ne", "n")])
}

def walk_grid(input_file):
    d = {"n" : 0, "ne" : 0, "se" : 0, "s" : 0, "sw" : 0, "nw" : 0}
    with open("input") as f:
        s = f.read().strip().split(",")
    m = max(funcs[i](d) for i in s)
    return d, m


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 11')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    d, m = walk_grid(args.input)
    # print(d)
    print("Part 1:", sum(d.values()))
    print("Part 2:", m)

