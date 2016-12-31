
from collections import Counter

def is_valid_room(s):
    p = s.rfind('-')
    q = s.find('[')
    name = s[:p].translate(None, '-')
    sid = s[p+1:q]
    h = s[q+1:-1]
    c = Counter(name).items()
    c.sort(key=lambda x: x[0])
    c.sort(key=lambda x: x[1], reverse=True)
    test_h = "".join(map(lambda x: x[0], c))[:5]
    #print test_h
    if test_h == h:
        return True, sid
    else:
        return False, sid

with open('input4') as f:
    c = 0
    for l in f:
        t, sid = is_valid_room(l.strip())
        if t:
            c += int(sid)
    print c

