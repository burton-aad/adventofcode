#!/usr/bin/python

input19 = 3017957

n = input19
first = [1, 1]
last  = [n, 1]
t = 0

while n > 2:
    print "t", t, ", n", n, "->", first, last
    c = 2**t
    first[1] += c
    if n % 2 == 0:
        last[0] -= c
        last[1] += c
    else:
        first[0] += 2**(t+1)
        last[1] += first[1]
    n /= 2
    t += 1

print first[0], first[1] + last[1]
