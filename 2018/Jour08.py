#!/usr/bin/python

from __future__ import print_function
import sys


class Node:
    def __init__(self, id):
        self.id = id
        self.metadata = []
        self.child = []

    def sum(self):
        return sum(self.metadata) + sum(map(Node.sum, self.child))

    def value(self):
        if len(self.child) == 0:
            return self.sum()
        else:
            return sum([self.child[i-1].value() for i in self.metadata if i <= len(self.child)])

    def str(self):
        return "{}({}) -> {}".format(self.id, self.metadata, map(Node.str, self.child))

class Parser:
    def __init__(self, license):
        self.id = 0
        self.txt = license
        self.idx = 0

    def parse(self):
        n = Node(self.id)
        self.id += 1
        num_child = self.txt[self.idx]
        num_meta = self.txt[self.idx+1]
        self.idx += 2
        for _ in range(num_child):
            n.child.append(self.parse())
        n.metadata = self.txt[self.idx:self.idx+num_meta]
        self.idx += num_meta
        return n


def jour08(root):
    print("part 1:", root.sum())
    print("part 2:", root.value())

if __name__ == "__main__":
    input = "input08"
    if len(sys.argv) > 1:
        input = sys.argv[1]
    l = []
    with open(input) as f:
        txt = f.read()
        l = map(int, txt.split())
        # print(l)
        root = Parser(l).parse()
    # print(root.str())
    jour08(root)
