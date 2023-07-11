#!/usr/bin/python

import argparse


def caught(t, delay=0, stopAtcaught = False):
    wall = 0
    caught = 0
    for i in range(t[-1][0]+1):
        # print(i, wall)
        if i == t[wall][0]:
            depth = t[wall][1]-1
            scan_pos = (i+delay) % (2*depth)
            # print("test", scan_pos)
            if scan_pos > depth:
                scan_pos = depth - (scan_pos - depth)
            # print("pos", t[wall][0], ":", scan_pos)
            if scan_pos == 0:
                caught += t[wall][0] * t[wall][1]
                if stopAtcaught:
                    return caught + 1
            wall += 1
    return caught

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 13')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    layers = []
    with open("input") as f:
        for l in f:
            layers.append(tuple(map(int, l.strip().split(": "))))
    # print(layers)

    print("Part 1:", caught(layers))

    i = 0
    while caught(layers, i, True):
        i += 1
    print("Part 2:", i)
