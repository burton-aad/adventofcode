
pad = ((1,2,3), (4,5,6), (7,8,9))
row = 1
col = 1

with open("input2") as f:
    for l in f:
        if len(l) == 0:
            continue
        for d in l:
            if d == 'U':
                row = row-1 if row > 0 else row
            elif d == 'R':
                col = col+1 if col < 2 else col
            elif d == 'L':
                col = col-1 if col > 0 else col
            elif d == 'D':
                row = row+1 if row < 2 else row
        print pad[row][col]

