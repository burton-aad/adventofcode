
from __future__ import print_function
from pprint import pprint


right = 0
up = 1
left = 2
down = 3

def next_v_1(s, x, y, v):
    return v + 1

def create_spiral_tour(tour, limit=0, next_v=next_v_1):
    line_w = 2*(tour)+1
    s = [[0] * line_w for _ in range(line_w)]
    x, y = line_w/2, line_w/2
    # set 1
    s[x][y] = 1
    y += 1
    v = 1
    for i in range(1, tour+1):
        n = 8*i
        for j in range(n):
            v = next_v(s, x, y, v)
            s[x][y] = v
            if limit > 0 and v >= limit:
                return s, x, y
            if j < n/4-1:
                x -= 1 # go up
            elif j < n/2-1:
                y -= 1 # go left
            elif j < 3*n/4-1:
                x += 1 # go down
            else:
                y += 1 # go right
    return s, x, y-1 # one extra step on right

# pprint(create_spiral_tour(0))
# pprint(create_spiral_tour(3))

def step_from_value(v):
    i = 0
    r = 1
    while r < v:
        i += 1
        r += 8*i
    return i

def get_1_dist(v):
    s, x, y = create_spiral_tour(step_from_value(v), v)
    pos1 = len(s[0])/2
    return abs(x - pos1) + abs(y - pos1)

# out 1
print(get_1_dist(368078))


def next_v_2(s, x, y, v):
    return s[x-1][y-1] + s[x][y-1] + s[x+1][y-1] + s[x-1][y] + s[x+1][y] + s[x-1][y+1] + s[x][y+1] + s[x+1][y+1]

def get_2_val(v):
    s, x, y = create_spiral_tour(300, v, next_v_2)
    return s[x][y]

# pprint(create_spiral_tour(5, 1000, next_v_2))

# out 2
print(get_2_val(368078))
