#!/usr/bin/env python

from __future__ import print_function
import sys
import itertools
from intcomp import Intcode

def main(v):
    p = Intcode(v)
    p.init(12, 2)
    p.run()
    # print(p)
    print("part 1 :", p.prog[0])

    # seek for specific value
    target = 19690720
    for i, j in itertools.product(range(100), repeat=2):
        p.reset()
        p.init(i, j)
        p.run()
        if p.prog[0] == target:
            print("part 2 : {} (100 * {} + {})".format(100*i+j, i, j))


# main([1,9,10,3,2,3,11,0,99,30,40,50])
# main([2,4,4,5,99,0])
# main([1,1,1,4,99,5,6,0,99])

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage : {} <input>".format(sys.argv[0]))
        sys.exit(1)
    with open(sys.argv[1]) as f:
        v = [int(x) for x in f.read().strip().split(',')]
        main(v)
