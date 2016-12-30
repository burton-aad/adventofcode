
from collections import Counter

def max_letter(t):
    c = Counter(t)
    return c.most_common(1)[0][0]


with open('input6') as f:
    m = zip(*f.readlines())
    t = []
    for l in m:
        t.append(max_letter(l))
    print "".join(t)
