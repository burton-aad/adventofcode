#!/usr/bin/python3

from hashlib import md5
import re
import argparse

def parse_keys(l, k, i, h):
    # to validate or not
    r = []
    for li,ls in l:
        if i > li + 1000:
            # not valid
            pass
        elif ls*5 in h:
            # valid
            k.append((li, ls, i))
            # print("key", li, "validate with hash", i)
        else:
            # keep it in the list
            r.append((li,ls))
    return r, k

def get_keys(salt, f_hash=lambda s,i: md5("{}{}".format(s, i).encode()).hexdigest()):
    # generate the index of all keys
    k = []
    l = []
    i = 0
    r = re.compile(r"(.)\1\1")
    while len(k) < 64:
        h = f_hash(salt, i)
        l, k = parse_keys(l, k, i, h)
        m = r.search(h)
        if m is not None:
            l.append((i, m.group(1)))
        i += 1
    return k

def h2(s, i):
    h = md5("{}{}".format(s, i).encode()).hexdigest()
    for i in range(2016):
        h = md5(h.encode()).hexdigest()
    return h

salt = "jlmsuwbz" # input
input13 = 1362
parser = argparse.ArgumentParser(description='AoC 2016 - Jour 14')
parser.add_argument("salt", nargs='?', help="office designer's favorite number", default=salt)
args = parser.parse_args()

# part 1
k = get_keys(args.salt)
print("end k", k)
print("Cas 1: 64 ->", k[63])
print("Part 1:", k[63][0])

# part 2
k = get_keys(args.salt, h2)
print("end k", k)
print("Cas 2: 64 ->", k[63])
print("Part 2:", k[63][0])
