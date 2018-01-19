
from __future__ import print_function

HORZ = 0
VERT = 1
pos = (0, 0)
all_pos = []
check_cross = True
axe = 'N'
rose = "NESW"
move = {
    'N' : lambda pos, length: ( pos[HORZ], pos[VERT] + length ),
    'E' : lambda pos, length: ( pos[HORZ] + length, pos[VERT] ),
    'S' : lambda pos, length: ( pos[HORZ], pos[VERT] - length ),
    'W' : lambda pos, length: ( pos[HORZ] - length, pos[VERT] ),
}

def input_move(turn, length):
    global axe, pos, all_pos, check_cross
    i = rose.index(axe)
    if turn == 'R':
        i = (i+1) % len(rose)
    else: # turn == 'L'
        i = (i-1) % len(rose)
    axe = rose[i]
    if check_cross:
        old_pos = pos
        for i in range(length):
            new_pos = move[axe](old_pos, i)
            if new_pos in all_pos:
                print("First cross at", new_pos, ":", abs(new_pos[0]) + abs(new_pos[1]), "Blocks away")
                check_cross = False
                break
            else:
                all_pos.append(new_pos)
    pos = move[axe](pos, length)

with open("input1") as f:
    txt = f.read().replace(",", " ").split()
    # print(txt)
    for m in txt:
        # print("move :", m[0], int(m[1:]))
        input_move(m[0], int(m[1:]))
    print(pos)
    print("The Easter Bunny HQ is", abs(pos[0]) + abs(pos[1]), "Blocks away")
