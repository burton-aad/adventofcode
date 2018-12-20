#!/usr/bin/python

from __future__ import print_function
import sys
import re

class Node:
    def __init__(self, id):
        self.id = id
        self.parent = []
        self.child = []
        self.wait = 0

    def set_child(self, node):
        self.child.append(node)
        node.parent.append(self)
        node.wait+=1

    def reset(self):
        self.wait = len(node.parent)

    def str(self):
        return "{} -> {}".format(self.id, self.child)

    def __str__(self):
        return "{}".format(self.id)
    def __repr__(self):
        return self.__str__()
    def __hash__(self):
        return ord(self.id)


def run_largeur_sort(roots):
    c = sorted(roots, key=lambda x: x.id, reverse=True)
    r = []
    while len(c) > 0:
        n = c.pop()
        if n.wait > 1:
            n.wait -= 1
        else:
            r.append(n.id)
            c.extend(n.child)
            c.sort(key=lambda x: x.id, reverse=True)
    return r


def jour07(roots):
    print("part 1:", "".join(run_largeur_sort(roots)))


# if __name__ == "__main__":
if True:
    input = "input07"
    if len(sys.argv) > 1:
        input = sys.argv[1]
    r = re.compile("Step (\w+) must be finished before step (\w+) can begin.")
    d = {}
    with open(input) as f:
        for l in f:
            m = r.match(l)
            p = m.group(1)
            c = m.group(2)
            d.setdefault(p, Node(p)).set_child(d.setdefault(c, Node(c)))
    roots = []
    for i,n in d.items():
        if len(n.parent) == 0:
            roots.append(n)
    print("roots :", roots)
    jour07(roots)
