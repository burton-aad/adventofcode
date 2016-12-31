
count = 0

def is_valid_triangle(a, b, c):
    return a+b > c and b+c > a and a+c > b

with open("input3") as f:
    for l in f:
        a,b,c = map(int, l.split())
        if is_valid_triangle(a, b, c):
            count += 1
    print count
