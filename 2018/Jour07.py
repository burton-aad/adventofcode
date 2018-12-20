#!/usr/bin/python

from __future__ import print_function
import sys
import re

class Node:
    def __init__(self, id):
        self.id = id
        self.parent = []
        self.child = []
        self.reset()

    def set_child(self, node):
        self.child.append(node)
        node.parent.append(self)
        node.wait+=1

    def reset(self):
        self.wait = len(self.parent)
        self.time = ord(self.id) - ord('@') + 60
        for n in self.child:
            n.reset()

    def str(self):
        return "{} -> {}".format(self.id, self.child)

    def __str__(self):
        return "{}".format(self.id)
    def __repr__(self):
        return self.__str__()
    def __hash__(self):
        return ord(self.id)


def run_largeur_sort(roots, workers=1):
    queue = sorted(roots, key=lambda x: x.id, reverse=True)
    run = []
    end = []
    time = 0
    while len(queue) > 0 or len(run) > 0:
        # print("queue {}, run {}, workers {}, time {}".format(queue, run, workers, time))
        if len(queue) > 0 and workers > 0:
            n = queue.pop()
            if n.wait > 1:
                n.wait -= 1
            else:
                run.append(n)
                workers -= 1
        else:
            # wait workers
            n = min(run, key=lambda n: n.time)
            run.remove(n)
            end.append(n.id)
            workers += 1
            time += n.time
            for o in run:
                o.time -= n.time
            queue.extend(n.child)
            queue.sort(key=lambda x: x.id, reverse=True)
    return end, time


def jour07(roots):
    n_order, _ = run_largeur_sort(roots)
    print("part 1:", "".join(n_order))
    for n in roots:
        n.reset()

    _, time = run_largeur_sort(roots, 5)
    print("part 2: {} secs".format(time))


if __name__ == "__main__":
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
