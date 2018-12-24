#!/usr/bin/python

from __future__ import print_function
import sys
import numpy as np


def find_max(g, size=3):
    # print(size) # progress log
    n = g.shape[0]
    subg = [np.sum(g[i:i+size, j:j+size]) for i in range(n-size+1) for j in range(n-size+1)]
    # print(subg)
    t = max(subg)
    i = subg.index(t)
    return t, ((i % (n-size+1)) + 1, (i / (n-size+1)) + 1)


def jour11(serial, gs):
    g = np.zeros((gs,gs))
    rackId= np.arange(1, gs+1) + 10
    g += rackId * np.arange(1, gs+1).reshape(gs,1) + serial
    g *= rackId
    g = ((g % 1000) // 100) - 5
    # print("\n{}".format(g))
    print("part 1: region {} with power {}".format(*find_max(g)[::-1]))
    t = [find_max(g, n+1) for n in range(gs)]
    p2 = max(t, key=lambda x: x[0])
    print("part 2: region {}, size {}, with power {}".format(p2[1], t.index(p2)+1, p2[0]))


if __name__ == "__main__":
    serial = 5034
    if len(sys.argv) > 1:
        serial = int(sys.argv[1])
    jour11(serial, 300)


