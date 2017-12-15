#!/usr/bin/python


input15 = (783, 325)


def generate_next(n, factor):
    return (n * factor) % 2147483647

gen_A = lambda x: generate_next(x, 16807)
gen_B = lambda x: generate_next(x, 48271)

def match_pairs(a, b):
    return (a & 0xFFFF) == (b & 0xFFFF)

# s = (65, 8921)
s = input15
c = 0
for _ in range(40000000):
    i,j = s
    s = (gen_A(i), gen_B(j))
    if match_pairs(*s):
        c += 1
    # print s, "->", match_pairs(*s)

print c

s = input15
c = 0
for _ in range(5000000):
    i,j = s
    i = gen_A(i)
    while i % 4 != 0:
        i = gen_A(i)
    j = gen_B(j)
    while j % 8 != 0:
        j = gen_B(j)
    s = (i, j)
    if match_pairs(*s):
        c += 1
    # print s, "->", match_pairs(*s)

print c
