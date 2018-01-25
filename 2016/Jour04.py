
from collections import Counter
import re

r = re.compile("([a-z-]+)-([0-9]+)\[([a-z]+)\]")

def extract(s):
    global r
    m = r.match(s)
    return m.groups()

def is_valid_room(name, h):
    c = Counter(name).items()
    c.sort(key=lambda x: x[0])
    c.sort(key=lambda x: x[1], reverse=True)
    test_h = "".join(map(lambda x: x[0], c))[:5]
    #print test_h
    return test_h == h

def real_name(name, sid):
    s = ""
    for c in name:
        if c == '-':
            s += c
        else:
            c = ord(c) - ord('a')
            c = (c + sid) % 26
            c = chr(c + ord('a'))
            s += c
    return s

with open('input04') as f:
    c = 0
    for l in f:
        l = l.strip()
        if len(l) == 0:
            continue
        name, sid, h = extract(l)
        if is_valid_room(name.translate(None, '-'), h):
            c += int(sid)
        # grep north on output
        print real_name(name, int(sid)), sid
    print c

