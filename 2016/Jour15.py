#!/usr/bin/python

import itertools

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


d = []
# d.append(disk(1, 5, 4))
# d.append(disk(2, 2, 1))

# input
# Disc #1 has 17 positions; at time=0, it is at position 1.
# Disc #2 has 7 positions; at time=0, it is at position 0.
# Disc #3 has 19 positions; at time=0, it is at position 2.
# Disc #4 has 5 positions; at time=0, it is at position 0.
# Disc #5 has 3 positions; at time=0, it is at position 0.
# Disc #6 has 13 positions; at time=0, it is at position 5.
d.append(disk(1, 17, 1))
d.append(disk(2, 7))
d.append(disk(3, 19, 2))
d.append(disk(4, 5))
d.append(disk(5, 3))
d.append(disk(6, 13, 5))

# Cas 2
d.append(disk(7, 11))

time = d[0].next_time_through()
print time
while not is_falling(time, d):
    time = d[0].next_time_through(time+1)
    print time

print "first fall time:", time
