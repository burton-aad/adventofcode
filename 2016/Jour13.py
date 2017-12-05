#!/usr/bin/python

from pprint import pprint

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

def print_maze(m, j=""):
    n = len(str(len(m)))
    print " "*n, "".join(map(str, range(len(m[0]))))
    for i,t in enumerate(m):
        print "{0:>{1}}".format(i, n),
        print j.join(map(str, t))


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


# m = create_maze(10, 10, 7)
# print
# print_maze(m)
# run_maze(m)
# pprint(m)
# print m[4][7]

input = 1362
m = create_maze(input, 32, 40)
print
print_maze(m)
run_maze(m)
print_maze(m, " ")

# step 1
print m[39][31]

# step 2
n = 0
for l in m:
    for v in l:
        try:
            if int(v) <= 50:
                n += 1
        except:
            pass
print n
