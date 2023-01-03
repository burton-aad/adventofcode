#!/usr/bin/python3

import argparse


def make_blacklist(filename):
    b = []
    with open(filename) as f:
        for l in f:
            b.append(list(map(int, l.split("-"))))
    b.sort(key=lambda x: x[0])
    p = 0
    end = len(b)-1
    while p < end:
        if b[p][1]+1 >= b[p+1][0]:
            if b[p+1][1] > b[p][1]:
                b[p][1] = b[p+1][1]
            b.pop(p+1)
            end -= 1
        else:
            p += 1
    return b

def reverse_list(l, maximum):
    r = []
    if l[0][0] > 0:
        r.append([0, l[0][0]-1])
    for i in range(len(l)-1):
        r.append([l[i][1]+1, l[i+1][0]-1])
    if l[-1][1] < maximum:
        r.append([l[-1][1]+1, maximum])
    return r

parser = argparse.ArgumentParser(description='AoC 2016 - Jour 20')
parser.add_argument("input", nargs='?', help="input file", default="input")
args = parser.parse_args()

blacklist = make_blacklist("input")
# print(blacklist)

ip = reverse_list(blacklist, 2**32-1)
# print(ip)

print("low", ip[0][0])

somme = 0
for s,e in ip:
    somme += e-s+1
print("Part 2:", somme)
