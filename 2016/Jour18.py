#!/usr/bin/python

def next_line(line):
    r = []
    t = [False] + line + [False]
    for i in range(len(line)):
        r.append( (t[i] and not t[i+2]) or (not t[i] and t[i+2]) )
    return r

def theme(l):
    return map(lambda x: x=='^', l)

def version(l):
    return "".join(map(lambda x: '^' if x else '.', l))

input18 = ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

l = theme(input18)
c = l.count(False)
for i in range(39):
    l = next_line(l)
    c += l.count(False)

print c

l = theme(input18)
c = l.count(False)
for i in range(400000-1):
    l = next_line(l)
    c += l.count(False)

print c
