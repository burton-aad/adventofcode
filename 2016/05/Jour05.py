#!/usr/bin/env python3

import argparse
import hashlib

input5 = "ojvtpuvg"


def main(door_id):
    codeA, codeB = [], ['-'] * 8
    index = 0
    while '-' in codeB:
        h = hashlib.md5((door_id+str(index)).encode()).hexdigest()
        if h.startswith("00000"):
            if len(codeA) < 8:
                codeA.append(h[5])
            i = int(h[5], 16)
            if i < len(codeB) and codeB[i] == '-':
                codeB[i] = h[6]
            print(index, h, h[5], codeB)
        index += 1
    return codeA, codeB


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2016 - Jour 05')
    parser.add_argument("input", nargs='?', help="door id", default=input5)
    args = parser.parse_args()

    codeA, codeB = main(args.input)
    print("A:", "".join(codeA))
    print("B:", "".join(codeB))
