
import hashlib

input5 = "ojvtpuvg"

code = []
index = 0

while len(code) < 8:
    h = hashlib.md5(input5+str(index))
    if h.hexdigest().startswith("00000"):
        code.append(h.hexdigest()[5])
        print index, h.hexdigest(), code[-1]
    index += 1

print "".join(code)
