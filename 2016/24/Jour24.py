#!/usr/bin/python3

import itertools
import argparse
from collections import deque

def print_v(*args):
    pass

def make_maze(filename):
    with open(filename) as f:
        maze = []
        pos = {}
        for i, s in enumerate(f):
            l = []
            for j, c in enumerate(s.strip()):
                l.append(c)
                if c != '.' and c != '#':
                    pos[int(c)] = (i,j)
            maze.append(l)
    rpos = []
    for i in range(max(pos)+1):
        rpos.append(pos[i])
    return rpos, maze

def print_maze(m):
    for l in m:
        print("".join(map(str, l)))

def maze_dist(maze, p, nb_elt):
    m = [r[:] for r in maze]
    m[p[0]][p[1]] = 'X'
    d = [0] * nb_elt
    f = deque([(0, p)])
    while len(f) > 0:
        n, (x,y) = f.popleft()
        for i,j in ((x+1, y), (x-1, y), (x, y+1), (x, y-1)):
            if m[i][j] == '.':
                m[i][j] = 'X'
                f.append((n+1, (i,j)))
            elif '0' <= m[i][j][0] <= '9':
                d[int(m[i][j])] = n+1
                m[i][j] = 'X'
                f.append((n+1, (i,j)))
    return d

def min_step(m, d, nb_elt, end_point=None):
    rmin = 100000000
    if end_point is not None:
        end = (end_point,)
    else:
        end = ()
    for p in itertools.permutations(range(1, nb_elt)):
        ns = 0
        print_v(p, end)
        pos = 0
        for v in p + end:
            print_v("dist", pos, ",", v, ":", d[pos][v])
            ns += d[pos][v]
            pos = v
        print_v(ns)
        if ns < rmin:
            rmin = ns
    return rmin

parser = argparse.ArgumentParser(description='AoC 2016 - Jour 24')
parser.add_argument("input", nargs='?', help="input file", default="input")
parser.add_argument("-v", action="store_true", help="verbose")
args = parser.parse_args()

if args.v:
    print_v = print

num_pos, maze = make_maze(args.input)
print_maze(maze)
print("maze size {}x{}".format(len(maze), len(maze[0])))
print("Pos:", num_pos)

dists = []
for p in num_pos:
    d = maze_dist(maze, p, len(num_pos))
    dists.append(d)
print("Dists:", dists)

print("Part 1:", min_step(maze, dists, len(num_pos)))
print("Part 2:", min_step(maze, dists, len(num_pos), end_point=0))
