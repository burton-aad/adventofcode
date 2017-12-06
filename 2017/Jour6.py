#!/sur/bin/python

def disrtibute(lst):
    index = lst.index(max(lst))
    n = lst[index] / len(lst)
    p = lst[index] % len(lst)
    lst[index] = 0
    for i in range(len(lst)):
        j = (index + i + 1) % len(lst)
        if i < p:
            lst[j] = lst[j] + n + 1
        else:
            lst[j] = lst[j] + n
    return lst

def jour6(lst):
    d = []
    while lst not in d:
        d.append(lst[:])
        lst = disrtibute(lst)
    return lst, d

# input6 = [0, 2, 7, 0]
input6 = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

l,d = jour6(input6)
print len(d)-d.index(l), len(d)
