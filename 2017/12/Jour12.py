#!/usr/bin/python

import argparse


def group_from_pid(start_pid, t):
    f = [start_pid]
    lst_conn = set()
    while len(f) > 0:
        # print(f, " - ", lst_conn)
        pid = f.pop(0)
        if pid not in lst_conn:
            lst_conn.add(pid)
            f += t[pid]
    return lst_conn

def jour12(t):
    r = group_from_pid(0, t)
    # print(r)
    print("Part 1:", len(r))

    num_group = 0
    s = set(range(len(t)))
    while len(s) > 0:
        pid = s.pop()
        r = group_from_pid(pid, t)
        s = s.difference(r)
        # print(pid, " - ", r, " - ", s)
        num_group += 1
    print("Part 2:", num_group)


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 12')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    t = []
    with open("input") as f:
        for l in f:
            n = l.strip().split(" <-> ")[-1]
            t.append(tuple(map(int, n.split(", "))))
    # print(t)

    jour12(t)
