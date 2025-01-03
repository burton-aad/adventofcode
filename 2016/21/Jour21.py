#!/usr/bin/python

import argparse

class scrambler:
    def __init__(self, word):
        self.word = word
        self.t = list(word)

    def swap_pos(self, x, y):
        self.t[x], self.t[y] = self.t[y], self.t[x]

    def swap_letters(self, x, y):
        self.swap_pos(self.t.index(x), self.t.index(y))

    def rotate_right(self, x):
        self.rotate_left(len(self.t)-x)

    def rotate_left(self, x):
        self.t = self.t[x:] + self.t[:x]

    def rotate_based(self, x):
        i = self.t.index(x)
        if i >= 4:
            self.rotate_right(i+2)
        else:
            self.rotate_right(i+1)

    def reverse(self, x, y):
        self.t = self.t[:x] + list(reversed(self.t[x:y+1])) + self.t[y+1:]

    def move(self, x, y):
        self.t.insert(y, self.t.pop(x))

    def __repr__(self):
        return "".join(self.t)

    def rotate_based_rev(self, x):
        # a priori : ne marche que si len(l) > 8 et pair
        i = self.t.index(x)
        s = len(self.t)
        for j in range(s):
            if j >= 4 and (j+j+2) % s == i:
                self.rotate_left(j+2)
                break
            elif j < 4 and (j+j+1) % s == i:
                self.rotate_left(j+1)
                break

    def parse_reverse(self, line):
        l = line.strip().split()
        if l[0] == "swap":
            if l[1] == "position":
                self.swap_pos(int(l[2]), int(l[5]))
            else: # swap letters
                self.swap_letters(l[2], l[5])
        elif l[0] == "rotate":
            if l[1] == "right":
                self.rotate_left(int(l[2]))
            elif l[1] == "left":
                self.rotate_right(int(l[2]))
            else: # rotate based on position
                self.rotate_based_rev(l[6])
        elif l[0] == "reverse":
            self.reverse(int(l[2]), int(l[4]))
        else: # move
            self.move(int(l[5]), int(l[2]))

    def parse(self, line):
        l = line.strip().split()
        if l[0] == "swap":
            if l[1] == "position":
                self.swap_pos(int(l[2]), int(l[5]))
            else: # swap letters
                self.swap_letters(l[2], l[5])
        elif l[0] == "rotate":
            if l[1] == "right":
                self.rotate_right(int(l[2]))
            elif l[1] == "left":
                self.rotate_left(int(l[2]))
            else: # rotate based on position
                self.rotate_based(l[6])
        elif l[0] == "reverse":
            self.reverse(int(l[2]), int(l[4]))
        else: # move
            self.move(int(l[2]), int(l[5]))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 21')
    parser.add_argument("input", nargs='?', help="scrambler procedure", default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        process = f.readlines()

    sc = scrambler("abcdefgh")
    for l in process:
        sc.parse(l)
    print("scramble", sc.word, ":", sc)

    sc = scrambler("fbgdceah")
    for l in reversed(process):
        sc.parse_reverse(l)
    print("un-scramble", sc, ":", sc.word)
