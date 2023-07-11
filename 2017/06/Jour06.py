#!/usr/bin/python

import argparse

def distribute(lst):
    index = lst.index(max(lst))
    n = lst[index] // len(lst)
    p = lst[index] % len(lst)
    return tuple(n if i == index else v + n + ((i - index) % len(lst) <= p)
                 for i, v in enumerate(lst))

def jour6(lst):
    d = []
    lst = tuple(lst)
    while lst not in d:
        d.append(lst)
        lst = distribute(lst)
    return lst, d

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 06')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        input6 = [int(i) for i in f.readline().split()]
    # input6 = [0, 2, 7, 0]

    l,d = jour6(input6)
    print("Part 1:", len(d))
    print("Part 2:", len(d)-d.index(l))
