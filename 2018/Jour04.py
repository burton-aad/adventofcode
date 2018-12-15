#!/usr/bin/python

import sys
import re
from operator import add

WAKEUP = '.'
SLEEP = '#'
UNKNOWN = 'X'

class Guard:

    def __init__(self, id = -1):
        self.id = id
        self._status = list(UNKNOWN * 60)

    def falls_asleep(self, min):
        self._status[min] = SLEEP

    def wakes_up(self, min):
        self._status[min] = WAKEUP

    def sleep(t):
        while t >=0 and self._status[t] == UNKNOWN:
            t -= 1
        return self._status[t] == SLEEP if t >= 0 else False

    def time_sleep(self):
        a = 0
        r = 0
        for c in self._status:
            if c == SLEEP:
                a = 1
            elif c == WAKEUP:
                a = 0
            r += a
        return r

    def status(self):
        s = WAKEUP
        r = []
        for c in self._status:
            if c != UNKNOWN:
                s = c
            r.append(s)
        return "".join(r)

    def __str__(self):
        return "#{id}  {status}".format(id=self.id, status=self.status())

    def __repr__(self):
        return self.__str__()

def get_date_and_time(s):
    m = re.match(r"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] ", s)
    day = int(m.group(3))
    min = int(m.group(5))
    if m.group(4) != "00":
        day = int(m.group(3)) + 1
        min = 0
    return "{}-{}".format(day, m.group(2)), min

def get_shifts(f):
    regex = re.compile(r"Guard #(\d+) begins shift")
    shifts = {}
    for l in f:
        d,t = get_date_and_time(l)
        l = l.strip()[19:]
        m = regex.match(l)
        if m:
            shifts.setdefault(d, Guard()).id = m.group(1)
        elif l == "falls asleep":
            shifts.setdefault(d, Guard()).falls_asleep(t)
        elif l == "wakes up":
            shifts.setdefault(d, Guard()).wakes_up(t)
    # for k in sorted(shifts):
    #     print k, shifts[k]
    return shifts


def jour04(shifts):
    sleep = {}
    for g in shifts.values():
        if g.id not in sleep:
            sleep[g.id] = 0
        sleep[g.id] += g.time_sleep()
    grd = max(sleep.items(), key=lambda x : x[1])
    print grd
    most_sleep = [0] * 60
    for k,g in filter(lambda x: x[1].id == grd[0], shifts.items()):
        s = map(lambda x: x == SLEEP, g.status())
        most_sleep = map(add, most_sleep, s)
    m = most_sleep.index(max(most_sleep))
    print m
    print "part 1: guard {}, min {} -> {}".format(grd[0], m, int(grd[0]) * m)
        


def main(infile = "input04"):
    shifts = {}
    with open(infile) as f:
        shifts = get_shifts(f)
    jour04(shifts)
    

if __name__ == "__main__":
    if len(sys.argv) > 1:
        main(sys.argv[1])
    else:
        main()
