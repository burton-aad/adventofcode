
import sys
import argparse
import re
from collections import namedtuple

Play = namedtuple("Play", "red green blue")
play_default = [("red", 0), ("green", 0), ("blue", 0)]

def cmp_play(p, o):
    return p.red <= o.red and p.green <= o.green and p.blue <= o.blue

def parse_game(l):
    g, r = l.strip().split(":")
    p = []
    for x in r.split(";"):
        t = play_default + [(n, int(i)) for v in x.split(",") for i, n in [v.split()]]
        p.append(Play(**dict(t)))
    return int(g.split()[1]), p

def jour02(f):
    bag = Play(red=12, green=13, blue=14)
    s1 = 0
    s2 = 0
    for l in f:
        g, play = parse_game(l)
        if all(cmp_play(p, bag) for p in play):
            s1 += g
        r = max(p.red for p in play)
        g = max(p.green for p in play)
        b = max(p.blue for p in play)
        s2 += r * b * g
    print("Part 1:", s1)
    print("Part 2:", s2)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2023 - Jour 02')
    parser.add_argument('input_file', nargs='?', default="input", help='the input file')
    args = parser.parse_args(sys.argv[1:])
    with open(args.input_file) as f:
        jour02(f.readlines())
