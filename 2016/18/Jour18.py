#!/usr/bin/python

import argparse


def next_line(line):
    t = (False,) + line + (False,)
    return tuple((t[i] and not t[i+2]) or (not t[i] and t[i+2]) for i in range(len(line)))

def theme(l):
    return tuple(x=='^' for x in l)

def version(l):
    return "".join(map(lambda x: '^' if x else '.', l))


parser = argparse.ArgumentParser(description='AoC 2016 - Jour 18')
parser.add_argument("input", nargs='?', default="input")
parser.add_argument("-s", "--start", help="Start row if no input file")
parser.add_argument("-r", "--rows", nargs='+', type=int, default=[40, 400000],
                    help="Number of rows (multiple values possible)")
args = parser.parse_args()

if args.start:
    input18 = args.start
else:
    with open(args.input) as f:
        input18 = f.read().strip()

for i, r in enumerate(args.rows):
    l = theme(input18)
    c = l.count(False)
    for _ in range(r - 1):
        l = next_line(l)
        c += l.count(False)
    print("Part {}: {}".format(i+1, c))
