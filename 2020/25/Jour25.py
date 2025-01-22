#!/usr/bin/env python3

import sys
import argparse

def rfid_trans(val, subject):
    return (val * subject) % 20201227

def find_loop_size(target):
    s, v = 0, 1
    while v != target:
        v = rfid_trans(v, 7)
        s += 1
    return s

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 25')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        card = int(f.readline())
        door = int(f.readline())
    loop_card = find_loop_size(card)
    loop_door = find_loop_size(door)
    print(loop_card, loop_door)
    v = 1
    for _ in range(loop_card):
        v = rfid_trans(v, door)
    print("Part 1:", v)

