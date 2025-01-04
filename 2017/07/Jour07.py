#!/usr/bin/python

import re
import argparse

class Program:
    def __init__(self, name, weight):
        self.name = name
        self.weight = weight
        self.disk = []
        self.tot_w = 0

    def wgt(self):
        if self.tot_w == 0:
            self.tot_w = self.weight + sum(map(lambda x: x.wgt(), self.disk))
        return self.tot_w

    def __repr__(self):
        return self.name + "-" + str(self.wgt())

    def print_tree(self, prepend=""):
        print(prepend + self.name, self.wgt())
        for p in self.disk:
            p.print_tree(prepend+"  ")

def create_tree(fname):
    r = re.compile("(.*) \((\d+)\)( -> (.*))?")
    d = {}
    with open(fname) as f:
        for l in f:
            m = r.match(l)
            name = m.group(1)
            weight = int(m.group(2))
            if name in d:
                tree = d[name]
                tree.weight = weight
                del d[name]
            else:
                tree = Program(name, weight)
                d[name] = tree
            if m.group(4):
                hold = m.group(4).split(", ")
                for h in hold:
                    if h in d:
                        tree.disk.append(d[h])
                        del d[h]
                    else:
                        t = Program(h, 0)
                        d[h] = t
                        tree.disk.append(t)
    return next(iter(d.values()))


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 07')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    tree = create_tree(args.input)
    print("Part 1:", tree.name)
    # tree.print_tree()

    r = tree
    while True:
        s = sorted(r.disk, key=lambda x: x.wgt())
        # print(r, "->", s)
        if len(s) != 0:
            if s[0].wgt() == s[-1].wgt():
                break
            ref = s[0].wgt()
            r = s[-1]
        else:
            raise RuntimeError("Invalid tree")

    print(f"{r} is {r.weight}, should be {ref-r.wgt()+r.weight}")
    print(f"Part 2: {ref-r.wgt()+r.weight}")
