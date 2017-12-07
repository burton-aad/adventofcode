#!/usr/bin/python

from hashlib import md5
from itertools import compress

up = 0
down = 1
left = 2
right = 3

#       X (1,0)
#   Y    #########
#        #S| | | #
#        #-#-#-#-#
# (0,1)  # | | | #
#        #-#-#-#-#
#        # | | | #
#        #-#-#-#-#
#        # | | |
#        ####### V

def list_open(path):
    h = md5(path).hexdigest()
    return [ord(x) > ord('a') for x in h[:4]]

# print list_open("hijkl")

def possible_path(pos, l_open):
    if pos[0] == 0:
        l_open[left] = False
    if pos[0] == 3:
        l_open[right] = False
    if pos[1] == 0:
        l_open[up] = False
    if pos[1] == 3:
        l_open[down] = False
    return compress([(0, -1, "U"), (0, 1, "D"), (-1, 0, "L"), (1, 0, "R")], l_open)


# print list(possible_path((0,1), list_open("hijklD")))

test_1 = "ihgpwlah"
test_2 = "kglvqrro"
test_3 = "ulqzkmiv"
input17 = "pvhmgsws"


pos = (0,0)
l = [(pos, input17)]
while l and pos != (3, 3):
    pos, path = l.pop(0)
    for x,y,m in possible_path(pos, list_open(path)):
        l.append(((pos[0]+x, pos[1]+y), path+m))

print path[:8], path[8:]

test_4 = "ihgpwlah"
pos = (0,0)
l = [(pos, input17)]
while l:
    pos, path = l.pop(0)
    if pos == (3, 3):
        lpath = path
        continue
    for x,y,m in possible_path(pos, list_open(path)):
        l.append(((pos[0]+x, pos[1]+y), path+m))
    # print len(l)

print lpath[:8], len(lpath[8:]), lpath[8:]
