#!/usr/bin/python


t = []
with open("input12") as f:
    for l in f:
        n = l.strip().split(" <-> ")[-1]
        t.append(map(int, n.split(", ")))
# print t

def group_from_pid(start_pid):
    f = [start_pid]
    lst_conn = set()
    while len(f) > 0:
        # print f, " - ", lst_conn
        pid = f.pop(0)
        if pid not in lst_conn:
            lst_conn.add(pid)
            f += t[pid]
    return lst_conn

r = group_from_pid(0)
print len(r), r

num_group = 0
s = set(range(len(t)))
while len(s) > 0:
    pid = s.pop()
    r = group_from_pid(pid)
    s = s.difference(r)
    # print pid, " - ", r, " - ", s
    num_group += 1

print num_group
