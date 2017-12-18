

def spin_run(inp, n):
    t = [0]
    p = 0
    for s in range(1, n+1):
        p = ((p + inp) % len(t)) + 1
        t.insert(p, s)
        # print t
    return t

def spin_run_low(inp, n):
    p = 0
    t = 1
    r = 1
    for s in range(1, n+1):
        p = ((p + inp) % t) + 1
        if p == 1:
            r = s
            # print s
        t+=1
    return r

input17 = 312

t = spin_run(input17, 2017)

print t[t.index(2017)+1]

print spin_run_low(input17, 50000000)
