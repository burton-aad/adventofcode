#!/usr/bin/python

from __future__ import print_function
import sys
import re

class Point:
    def __init__(self, position, velocity):
        self.x = position[0]
        self.y = position[1]
        self.dx = velocity[0]
        self.dy = velocity[1]

    def tick(self):
        self.x += self.dx
        self.y += self.dy

    def untick(self):
        self.x -= self.dx
        self.y -= self.dy

    def __repr__(self):
        return "<{x},{y}>@<{dx},{dy}>".format(**self.__dict__)

def draw(pts):
    xmin, ymin, w, h = draw_size(pts)
    t = [['.' for _ in range(w)] for _ in range(h)]
    print("xmin {}, ymin {}, w {}, h {}".format(xmin, ymin, w, h))
    for p in pts:
        t[p.y - ymin][p.x - xmin] = '#'
    for l in t:
        print("".join(l))
    print()

def draw_size(pts):
    x = map(lambda p: p.x, pts)
    y = map(lambda p: p.y, pts)
    xmin = min(x)
    ymin = min(y)
    w = max(x) - xmin + 1
    h = max(y) - ymin + 1
    return xmin, ymin, w, h

def jour10(pts):
    # print(pts)
    _, _, w, h = draw_size(pts)
    rect = w*h
    tick = 0
    while True:
        map(Point.tick, pts)
        _, _, w, h = draw_size(pts)
        if rect < w*h:
            break
        rect = w*h
        tick += 1
    print("tick {}".format(tick))
    map(Point.untick, pts) # we gone one too far
    draw(pts)


if __name__ == "__main__":
    input = "input10"
    if len(sys.argv) > 1:
        input = sys.argv[1]
    r = re.compile("position=<\s*([-0-9]+),\s*([-0-9]+)> velocity=<\s*([-0-9]+),\s*([-0-9]+)>")
    pts = []
    with open(input) as f:
        for l in f:
            m = r.match(l)
            pts.append(Point((int(m.group(1)), int(m.group(2))), (int(m.group(3)), int(m.group(4)))))
    jour10(pts)
