
count = 0

def is_valid_triangle(a, b, c):
    return a+b > c and b+c > a and a+c > b

with open("input3") as f:
    l = f.readlines()
    l = map(lambda s: map(int, s.split()), l)
    l = zip(*l)
    t = l[0] + l[1] + l[2]
    for i in range(0, len(t), 3):
        if is_valid_triangle(*t[i:i+3]):
            count += 1
    print count
