#!/usr/bin/python3

from pprint import pprint
import argparse

def is_wall(x, y, n):
    s = x*x + 3*x + 2*x*y + y + y*y + n
    if bin(s).count('1') % 2 == 0:
        return '.'
    else:
        return '#'

def create_maze(num, w, h):
    t = []
    for i in range(h):
        l = []
        for j in range(w):
            l.append(is_wall(j, i, num))
        t.append(l)
    return t

def print_maze(m, j="", f=str):
    n = len(str(len(m)))
    print(" "*n, j.join(map(f, range(len(m[0])))))
    for i,t in enumerate(m):
        print("{0:>{1}} {2}".format(i, n, j.join(map(f, t))))


def run_maze(m):
    t = [(1,1,0)]
    while len(t) > 0:
        x,y,s = t.pop(0)
        if m[x][y] != '.':
            continue
        m[x][y] = str(s)
        for i,j in ((x+1, y), (x-1, y), (x, y+1), (x, y-1)):
            if 0 <= i < len(m) and 0 <= j < len(m[0]):
                t.append((i,j,s+1))


input13 = 1362
parser = argparse.ArgumentParser(description='AoC 2016 - Jour 13')
parser.add_argument("input", nargs='?', type=int, help="office designer's favorite number", default=input13)
parser.add_argument("-t", "--target", type=int, nargs=2, metavar=("x", "y"),
                    help="target to reach", default=[31, 39])
args = parser.parse_args()

m = create_maze(args.input, args.target[0]+1, args.target[1]+1)
# print_maze(m)
run_maze(m)
# print_maze(m, " ", "{:2}".format)

# step 1
print("Part 1:", m[args.target[1]][args.target[0]])

# step 2
n = 0
for l in m:
    for v in l:
        try:
            if int(v) <= 50:
                n += 1
        except:
            pass
print("Part 2:", n)
