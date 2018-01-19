
pad1 = ((1,2,3), (4,5,6), (7,8,9))
pad2 = (('*', '*', '1', '*', '*'), ('*', '2', '3', '4', '*'), ('5', '6', '7', '8', '9'), ('*', 'A', 'B', 'C', '*'), ('*', '*', 'D', '*', '*'))
pad = pad2
max_row = len(pad)-1
max_col = len(pad[0])-1
row = 1
col = 1

with open("input2") as f:
    for l in f:
        if len(l) == 0:
            continue
        for d in l:
            if d == 'U':
                row = row-1 if row > 0 and pad[row-1][col] != '*' else row
            elif d == 'R':
                col = col+1 if col < max_col and pad[row][col+1] != '*' else col
            elif d == 'L':
                col = col-1 if col > 0 and pad[row][col-1] != '*' else col
            elif d == 'D':
                row = row+1 if row < max_row and pad[row+1][col] != '*' else row
        print pad[row][col]

