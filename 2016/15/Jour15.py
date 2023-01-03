#!/usr/bin/python3

import re
import argparse

class disk:
    def __init__(self, num, npos, start_pos=0):
        self.num = num
        self.npos = npos
        self.start_pos = start_pos

    def pos(self, time):
        return (self.start_pos + time) % self.npos

    def pos_num(self, time):
        return self.pos(time+self.num)

    def next_time_through(self, time=0):
        t = time + self.npos - self.pos(time) - self.num
        if t < time:
            t += self.npos
        return t

def is_falling(time, disks):
    for d in disks:
        if d.pos_num(time) != 0:
            return False
    return True


parser = argparse.ArgumentParser(description='AoC 2016 - Jour 15')
parser.add_argument("input", nargs='?', help="input file", default="input")
args = parser.parse_args()

r = re.compile(r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).")
with open(args.input) as f:
    d = [disk(*map(int, r.match(l).groups())) for l in f]

time = d[0].next_time_through()
while not is_falling(time, d):
    time = d[0].next_time_through(time+1)
print("Part 1:", time)

# Cas 2
d.append(disk(7, 11))

time = d[0].next_time_through()
while not is_falling(time, d):
    time = d[0].next_time_through(time+1)

print("Part 2:", time)
