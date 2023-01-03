#!/usr/bin/env python3

from collections import Counter
import re
import argparse

r = re.compile("([a-z-]+)-([0-9]+)\[([a-z]+)\]")

def extract(s):
    m = r.match(s)
    return m.groups()

def is_valid_room(name, h):
    c = list(Counter(name).items())
    c.sort(key=lambda x: x[0])
    c.sort(key=lambda x: x[1], reverse=True)
    test_h = "".join(map(lambda x: x[0], c))[:5]
    return test_h == h

def real_name(name, sid):
    s = ""
    for c in name:
        if c == '-':
            s += c
        else:
            c = ord(c) - ord('a')
            c = (c + sid) % 26
            c = chr(c + ord('a'))
            s += c
    return s

def main(f):
    c = 0
    northp = ""
    for l in f:
        name, sid, h = extract(l)
        if is_valid_room(name.replace('-', ''), h):
            c += int(sid)
        rname = real_name(name, int(sid))
        if rname.startswith("north"):
            northp = f"{rname} {sid}"
    print("Part 1: ", c)
    print("Part 2: ", northp)

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 04')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        main(f)
