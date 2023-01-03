#!/usr/bin/env python3

import argparse

pad1 = (('1','2','3'), ('4','5','6'), ('7','8','9'))
pad1_start = (1, 1)
pad2 = (('*', '*', '1', '*', '*'), ('*', '2', '3', '4', '*'), ('5', '6', '7', '8', '9'), ('*', 'A', 'B', 'C', '*'), ('*', '*', 'D', '*', '*'))
pad2_start = (2, 0)

def move(pad, start, mv):
    max_row = len(pad)-1
    max_col = len(pad[0])-1
    row, col = start

    r = []
    for l in mv:
        for d in l:
            if d == 'U':
                row = row-1 if row > 0 and pad[row-1][col] != '*' else row
            elif d == 'R':
                col = col+1 if col < max_col and pad[row][col+1] != '*' else col
            elif d == 'L':
                col = col-1 if col > 0 and pad[row][col-1] != '*' else col
            elif d == 'D':
                row = row+1 if row < max_row and pad[row+1][col] != '*' else row
        r.append(pad[row][col])
    return "".join(r)

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 02')
    parser.add_argument("input", nargs='?', help="input file", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        moves = f.readlines()

    print("Part 1: ", move(pad1, pad1_start, moves))
    print("Part 2: ", move(pad2, pad2_start, moves))
