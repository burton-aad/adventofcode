#!/usr/bin/env python3

input = [20,0,1,11,6,3]

def Jour15(input):
    d = {v : i+1 for i, v in enumerate(input)}
    last = input[-1]
    age = len(input) + 1
    new = True

    for _ in range(2020 - len(input)):
        s = 0 if new else age - d[last] - 1
        d[last], last, age, new = age-1, s, age+1, s not in d
    print("Part 1:", last)

    # Un peu long :)
    for _ in range(30000000 - 2020):
        s = 0 if new else age - d[last] - 1
        d[last], last, age, new = age-1, s, age+1, s not in d
    print("Part 2:", last)


if __name__=="__main__":
    Jour15(input)
