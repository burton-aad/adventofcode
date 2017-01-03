
import re

WIDTH = 50
HEIGHT = 6
screen = [['.']*WIDTH for _ in range(HEIGHT)]

re_rect = re.compile("rect (\d+)x(\d+)")
re_rrow = re.compile("rotate row y=(\d+) by (\d+)")
re_rcol = re.compile("rotate column x=(\d+) by (\d+)")

def print_screen(s):
    for l in s:
        print "".join(l)


def rect(a, b):
    global screen
    for i in range(a):
        for j in range(b):
            screen[j][i] = '#'


def rotate_row(a, b):
    global screen
    l = screen[a]
    l = l[-b:] + l[:-b]
    screen[a] = l

def rotate_column(a,b):
    global screen, HEIGHT
    c = [screen[i][a] for i in range(HEIGHT)]
    c = c[-b:] + c[:-b]
    for i in range(HEIGHT):
        screen[i][a] = c[i]

# rect(3,2)
# rotate_column(1, 1)
# rotate_row(0, 4)
# rotate_column(1, 1)

with open("input8") as f:
    for l in f:
        m = re_rect.match(l)
        if m:
            rect(*map(int, m.groups()))
            continue
        m = re_rrow.match(l)
        if m:
            rotate_row(*map(int, m.groups()))
            continue
        m = re_rcol.match(l)
        if m:
            rotate_column(*map(int, m.groups()))
            continue
        print "Error in line", l

print
print_screen(screen)

c = 0
for l in screen:
    c += l.count('#')
print c
