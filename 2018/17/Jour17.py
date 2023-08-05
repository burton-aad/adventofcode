#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import re
import argparse
from collections import deque

SAND = '.'
WATER = '~'
FALLING_WATER = '|'
WALL = '#'

def print2D(tab):
    for l in tab:
        print("".join(l))

def pnull(*kargs):
    pass

pdebug = pnull

def parse_input(f, debug):
    regex = re.compile(r"([xy])=(\d+), [xy]=(\d+)..(\d+)")
    t = []
    for l in f:
        m = regex.match(l)
        e = ((int(m.group(2)),int(m.group(2))), (int(m.group(3)), int(m.group(4))))
        if m.group(1) == "x":
            t.append(e)
        else:
            t.append(tuple(reversed(e)))

    m = [deque("+")]
    mx, Mx = 500, 500
    my = 0
    for px, py in t:
        pdebug("(", px, ",", py, ")")
        pdebug("x [{},{}], y [{},{}]".format(mx, Mx, 0, my))
        xmin, xmax = px
        if xmin < mx:
            for l in m:
                l.extendleft('.'* (mx - xmin))
            mx = xmin
        if xmax > Mx:
            for l in m:
                l.extend('.'* (xmax - Mx))
            Mx = xmax
        ymin, ymax = py
        if ymax > my:
            for _ in range(ymax - my):
                m.append(deque('.'* len(m[0])))
            my = ymax
        for i in range(ymin, ymax+1):
            for j in range(xmin-mx, xmax-mx+1):
                m[i][j] = "#"
    # add one column in both side + one line in the bottom
    return [['.'] + list(x) + ['.'] for x in m] + [list('.' * (2+len(m[0])))]


def search_fill_area(grnd, sx, ex, py):
    r = []
    fall = []
    for x,u in zip((sx, ex), (-1, 1)):
        # Do we fall ?
        while (grnd[py+1][x] == WALL or grnd[py+1][x] == WATER) and grnd[py][x] != WALL:
            x += u
        if grnd[py][x] != WALL:
            # yes, we fall again
            fall.append((x, py))
        r.append(x) # or new wall appears
    sx, ex = r
    return fall, sx, ex

def fill_water(grnd, px, py):
    fall, sx, ex = search_fill_area(grnd, px, px, py)
    while not fall:
        # fall on left or right
        if grnd[py][sx] != WALL or grnd[py][ex] != WALL:
            pdebug("fall ?", py)
            fall, sx, ex = search_fill_area(grnd, sx, ex, py)
            pdebug("fall again", fall, (sx, ex))
            if fall:
                # fill the current falling position
                for x in range(sx+1, ex):
                    grnd[py][x] = FALLING_WATER
        elif WALL in grnd[py][sx+1:ex]:
            # internal wall
            fall.extend(fill_water(grnd, px, py))
        else:
            # fill water
            pdebug("fill", py)
            for x in range(sx+1, ex):
                grnd[py][x] = WATER
            py -= 1
    return fall

def fall_water(grnd):
    q = deque([(grnd[0].index('+'), 1)])
    while len(q) > 0:
        px, py = q.popleft()
        n = grnd[py][px]
        pdebug("({}, {}) -> {}".format(px, py, n))
        if n == SAND:
            grnd[py][px] = FALLING_WATER
            if py+1 < len(grnd):
                q.append((px, py+1))
        elif n == FALLING_WATER:
            pass
        elif n == WALL and grnd[py][px-1] == SAND and grnd[py][px+1] == SAND:
            q.extend([(px-1, py-1), (px+1, py-1)])
        else:
            q.extend(fill_water(grnd, px, py-1))
            pdebug("-"*100)
            # print2D(grnd)
            pdebug(q)

def water_count(grnd, cnt_vals):
    sy = 0
    while WALL not in grnd[sy]:
        sy += 1
    count = 0
    for c in cnt_vals:
        for y in range(sy, len(grnd)-1):
            count += grnd[y].count(c)
    return count


def jour17(f, debug=False):
    detect = parse_input(f, debug)
    fall_water(detect)
    print("-"*100)
    print2D(detect)
    print("Part 1:", water_count(detect, [FALLING_WATER, WATER]))
    print("Part 2:", water_count(detect, [WATER]))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2018 - Jour 17')

    parser.add_argument('input_file', nargs='?', default="input", help='the input file')
    parser.add_argument('-d', "--debug", action='store_true', help="print debug")

    args = parser.parse_args(sys.argv[1:])
    if args.debug:
        pdebug = print

    with open(args.input_file) as f:
        jour17(f)


# test
# jour17(["x=495, y=2..7"])
# jour17(["x=495, y=2..7", "y=7, x=495..501", "x=501, y=3..7", "x=498, y=2..4", "x=506, y=1..2", "x=498, y=10..13", "x=504, y=10..13", "y=13, x=498..504"])
# with open("input") as f:
#     d = []
#     for l in f:
#         d.append(list(l.strip()))
#     fall_water(d)
#     print()
#     print2D(d)
