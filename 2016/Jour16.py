#!/usr/bin/python

def generate_data(d):
    e = map(lambda x: x^1, d)
    e.reverse()
    return d + [0] + e

# print generate_data([1])
# print generate_data([0])
# print generate_data([1,1,1,1,1])
# print generate_data([1,1,1,1,0,0,0,0,1,0,1,0])

def checksum(d):
    while len(d) % 2 == 0:
        c = []
        for i in range(len(d)/2):
            if d[2*i] == d[2*i+1]:
                c.append(1)
            else:
                c.append(0)
        d = c
    return d

# print checksum([1,1,0,0,1,0,1,1,0,1,0,0])
# print checksum([1,0,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1])


input_len1 = 272
input_len2 = 35651584
input16 = [1,0,0,1,1,1,1,1,0,1,1,0,1,1,0,0,1]

d = input16
while len(d) < input_len1:
    d = generate_data(d)
print "".join(map(str, checksum(d[:input_len1])))

d = input16
while len(d) < input_len2:
    d = generate_data(d)
print "".join(map(str, checksum(d[:input_len2])))
