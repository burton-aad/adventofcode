
from __future__ import print_function
from pprint import pprint


right = 0
up = 1
left = 2
down = 3

def step_from_value(v):
    i = 0
    r = 1
    while r < v:
        i += 1
        r += 8*i
    return i

def create_spiral_tour(tour=0):
    line_w = 2*(tour)+1
    s = [[0] * line_w for _ in range(line_w)]
    x, y = line_w/2, line_w/2
    # set 1
    s[x][y] = 1
    y += 1
    v = 2
    for i in range(1, tour+1):
        n = 8*i
        for j in range(n):
            s[x][y] = v
            v += 1
            if j < n/4-1:
                x -= 1 # go up
            elif j < n/2-1:
                y -= 1 # go left
            elif j < 3*n/4-1:
                x += 1 # go down
            else:
                y += 1 # go right
    return s

def create_spiral(v):
    return create_spiral_tour(step_from_value(v))

def get_val_pos(s, v):
    n = len(s[0])
    for i in range(n):
        for j in range(n):
            if s[i][j] == v:
                return i, j

def get_1_dist(v):
    s = create_spiral(v)
    x, y = get_val_pos(s, v)
    pos1 = len(s[0])/2
    return abs(x - pos1) + abs(y - pos1)

# out 1
print(get_1_dist(368078))
