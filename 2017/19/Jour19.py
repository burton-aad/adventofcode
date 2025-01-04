#!/usr/bin/python

from pprint import pprint
import argparse

def next_dir(pos, x, y):
    i, j = pos
    return i+x, j+y

left  = lambda x: next_dir(x, 0, -1)
right = lambda x: next_dir(x, 0, 1)
up    = lambda x: next_dir(x, -1, 0)
down  = lambda x: next_dir(x, 1, 0)

def get(a, x, y):
    return a[x][y]

sym_lines = "|-+"

def main(lines):
    t = []
    step = 0
    pos = (0, lines[0].find('|'))
    direction = down
    c = get(lines, *pos)

    while c != " ":
        npos = direction(pos)
        # print npos
        nc = get(lines, *npos)
        # print nc
        if nc == " ":
            for d in [left, right] if direction in [up, down] else [up, down]:
                if get(lines, *d(pos)) != " ":
                    direction = d
                    npos = direction(pos)
                    nc = get(lines, *npos)
                    break
            # print npos, "->", nc
        if nc not in sym_lines:
            t.append(nc)
        pos = npos
        c = nc
        step += 1

    return step, t

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 19')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open("input") as f:
        lines = [l.strip("\n") for l in f]
        lines.append(" "*len(lines[0]))
        # pprint(lines)

    step, t = main(lines)
    print("Part 1:", "".join(t))
    print("Part 2:", step, "steps")
