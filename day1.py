
from __future__ import print_function

HORZ = 0
VERT = 1
pos = (0, 0)
axe = 'N'
rose = "NESW"
move = {
    'N' : lambda pos, length: ( pos[HORZ], pos[VERT] + length ),
    'E' : lambda pos, length: ( pos[HORZ] + length, pos[VERT] ),
    'S' : lambda pos, length: ( pos[HORZ], pos[VERT] - length ),
    'W' : lambda pos, length: ( pos[HORZ] - length, pos[VERT] ),
}

def input_move(turn, length):
    global axe, pos
    i = rose.index(axe)
    if turn == 'R':
        i = (i+1) % len(rose)
    else: # turn == 'L'
        i = (i-1) % len(rose)
    axe = rose[i]
    pos = move[axe](pos, length)

with open("input1") as f:
    txt = f.read().replace(",", " ").split()
    # print(txt)
    for m in txt:
        # print("move :", m[0], int(m[1:]))
        input_move(m[0], int(m[1:]))
    print(pos)
    print("The Easter Bunny HQ is", abs(pos[0]) + abs(pos[1]), "Blocks away")
