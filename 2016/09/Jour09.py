#!/usr/bin/env python3

import argparse
import re
re_comp = re.compile("\((\d+)x(\d+)\)")


def decompress_v2(compress):
    d = compress
    while "(" in d:
        d = decompress(d)
    return d

def decompress(compress):
    d = ""
    s = compress
    while True:
        m = re_comp.search(s)
        if m is None:
            d += s
            break
        d += s[:m.start()]
        l = int(m.group(1))
        c = int(m.group(2))
        sub_s = s[m.end():m.end()+l]
        d += sub_s * c
        s = s[m.end()+l:]
    return d

def decompress_size(compress):
    size = 0
    s = compress
    while True:
        m = re_comp.search(s)
        if m is None:
            size += len(s)
            break
        size += len(s[:m.start()])
        l = int(m.group(1))
        c = int(m.group(2))
        sub_s = s[m.end():m.end()+l]
        size += decompress_size(sub_s) * c
        s = s[m.end()+l:]
    return size

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 09')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    parser.add_argument("-s", help="Pass string to test")
    args = parser.parse_args()

    if args.s:
        input9 = args.s
    else:
        with open(args.input) as f:
            input9 = f.read().strip()

    print("Part 1:", len(decompress(input9)))
    print("Part 2:", decompress_size(input9))
