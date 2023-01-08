#!/usr/bin/python

import os
import argparse
import re
import itertools
from collections import deque

class Part:
    def __init__(self, name, type):
        self.name = name
        self.type = type
        self.sh_name = None
        self.id = 0

    def __str__(self):
        return self.sh_name

    def __repr__(self):
        return self.name + self.type[0].upper()

def parse_input(f):
    floors = []
    r = re.compile(r"(\w+)(?:-compatible)? (microchip|generator)")
    for l in f:
        floors.append([Part(p.group(1), p.group(2)) for p in r.finditer(l)])

    parts = sorted(itertools.chain(*floors), key=lambda x: (x.name, x.type))
    for i, p in enumerate(parts):
        m = max(len(os.path.commonprefix([p.name.lower(), o.name.lower()]))
                for j, o in enumerate(parts) if i != j and o.type == p.type)
        p.sh_name = p.name[:m+1].capitalize() + p.type[0].upper()
        p.id = i
    return tuple(sum(1 << p.id for p in f) for f in floors), dict(enumerate(parts))

def bit_count(n):
    return bin(n).count('1')

def is_valid_floor(f):
    G = f & 0x55555555
    M = f & 0xaaaaaaaa
    return G == 0 or ((M ^ (G << 1)) & M) == 0

def hsh(floor):
    G = floor & 0x55555555
    M = floor & 0xaaaaaaaa
    return bit_count(M) + (bit_count(G) << 16) + (bit_count(M & (G << 1)) << 32)

def bfs(floors, max_id):
    cases = [1 << i | 1 << j for i, j in itertools.chain(((i, i) for i in range(max_id)),
                                                         itertools.combinations(range(max_id), 2))
             if is_valid_floor(1 << i | 1 << j)]
    target = (1 << max_id) - 1
    viewed = set()
    q = deque([(0, 0, list(floors))])

    while len(q) != 0:
        n, E, fs = q.popleft()
        # print(n, E, fs)
        if fs[-1] == target:
            return n
        n += 1

        for tp in cases:
            if (tp & fs[E]) == tp and is_valid_floor(fs[E] - tp):
                for nE in [E+1, E-1]:
                    if 0 <= nE < len(fs) and is_valid_floor(fs[nE] + tp):
                        nfs = fs[:]
                        nfs[E] -= tp
                        nfs[nE] += tp
                        h = tuple(hsh(f) for f in nfs)
                        if (nE, h) not in viewed:
                            # print("-> next", (n, nE, nfs))
                            q.append((n, nE, nfs))
                            viewed.add((nE, h))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 11')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        floors, parts = parse_input(f)
    # print(floors)
    # print(parts)

    max_id = max(parts.keys()) + 1
    print("Part 1:", bfs(floors, max_id))

    extra_parts = ((1 << 4) - 1) << max_id
    print("Part 2:", bfs((floors[0] + extra_parts,) + floors[1:], max_id + 4))
