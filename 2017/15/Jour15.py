#!/usr/bin/python

import argparse

 
def generate_next(n, factor):
    return (n * factor) % 2147483647

gen_A = lambda x: generate_next(x, 16807)
gen_B = lambda x: generate_next(x, 48271)

def match_pairs(a, b):
    return (a & 0xFFFF) == (b & 0xFFFF)

def jour15(a, b):
    c = 0
    for _ in range(40_000_000):
        a, b = (gen_A(a), gen_B(b))
        if match_pairs(a, b):
            c += 1
        # print(a, b, "->", match_pairs(a, b))
    return c


def jour15_2(a, b):
    c = 0
    for _ in range(5_000_000):
        # i,j = s
        a = gen_A(a)
        while a % 4 != 0:
            a = gen_A(a)
        b = gen_B(b)
        while b % 8 != 0:
            b = gen_B(b)
        if match_pairs(a, b):
            c += 1
        # print(a, b, "->", match_pairs(a, b))
    return c


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 15')
    parser.add_argument("A", nargs='?', type=int, default=783, help="Start gen A")
    parser.add_argument("B", nargs='?', type=int, default=325, help="Start gen B")
    args = parser.parse_args()

    print("Part 1:", jour15(args.A, args.B))
    print("Part 2:", jour15_2(args.A, args.B))
