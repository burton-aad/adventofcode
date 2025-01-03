#!/usr/bin/env python3

import argparse

HORZ = 0
VERT = 1
pos = (0, 0)
all_pos = []
cross_pos = None
axe = 'N'
rose = "NESW"
move = {
    'N' : lambda pos, length: ( pos[HORZ], pos[VERT] + length ),
    'E' : lambda pos, length: ( pos[HORZ] + length, pos[VERT] ),
    'S' : lambda pos, length: ( pos[HORZ], pos[VERT] - length ),
    'W' : lambda pos, length: ( pos[HORZ] - length, pos[VERT] ),
}

def input_move(turn, length):
    global axe, pos, all_pos, cross_pos
    i = rose.index(axe)
    if turn == 'R':
        i = (i+1) % len(rose)
    else: # turn == 'L'
        i = (i-1) % len(rose)
    axe = rose[i]
    if cross_pos is None:
        old_pos = pos
        for i in range(length):
            new_pos = move[axe](old_pos, i)
            if new_pos in all_pos:
                cross_pos = new_pos
                break
            else:
                all_pos.append(new_pos)
    pos = move[axe](pos, length)

def main(input_txt):
    for m in input_txt:
        # print("move :", m[0], int(m[1:]))
        input_move(m[0], int(m[1:]))
    # print(pos)
    print("Easter Bunny HQ :", abs(pos[0]) + abs(pos[1]), "Blocks away")
    print("First cross at", cross_pos, ":", abs(cross_pos[0]) + abs(cross_pos[1]), "Blocks away")

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 01')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        txt = f.read().replace(",", " ").split()
        # print(txt)
    main(txt)
