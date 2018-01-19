
import hashlib

input5 = "ojvtpuvg"

codeA = []
codeB = ['-'] * 8
index = 0

while '-' in codeB:
    h = hashlib.md5(input5+str(index)).hexdigest()
    if h.startswith("00000"):
        if len(codeA) < 8:
            codeA.append(h[5])
        i = int(h[5], 16)
        if i < len(codeB) and codeB[i] == '-':
            codeB[i] = h[6]
        print index, h, h[5], codeB
    index += 1

print "A:", "".join(codeA)
print "B:", "".join(codeB)
