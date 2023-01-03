#!/usr/bin/python3

import argparse

def generate_data(d):
    e = list(map(lambda x: x^1, d))
    e.reverse()
    return d + [0] + e

def checksum(d):
    while len(d) % 2 == 0:
        c = []
        for i in range(len(d)//2):
            if d[2*i] == d[2*i+1]:
                c.append(1)
            else:
                c.append(0)
        d = c
    return d


input_len1 = 272
input_len2 = 35651584
input16 = "10011111011011001"

parser = argparse.ArgumentParser(description='AoC 2016 - Jour 16')
parser.add_argument("input", nargs='?', help="initial state", default=input16)
parser.add_argument("-l", "--lengths", nargs='*', type=int, help="lengths", default=[input_len1, input_len2])
args = parser.parse_args()

for i, l in enumerate(args.lengths):
    d = [int(c) for c in args.input]
    while len(d) < l:
        d = generate_data(d)
    print("Part {}: {}".format(i+1, "".join(map(str, checksum(d[:l])))))
