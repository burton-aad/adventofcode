#!/usr/bin/python

from __future__ import print_function
from pprint import pprint
import sys
import argparse

try:
    from PIL import Image
except:
    Image = None

def rotate90(ls):
    return ["".join(x) for x in zip(*ls[::-1])]

def flip_h(ls):
    return ls[::-1]

def flip_v(ls):
    return ["".join(x) for x in zip(*list(zip(*ls))[::-1])]

def identity(ls):
    return ls

def create_dict(infile):
    d = {}
    with open(infile) as f:
        for l in f:
            k,v = [x.split("/") for x in l.strip().split(" => ")]
            d[tuple(k)] = v
    # get all variation
    for k in list(d):
        for f in [identity, flip_h, flip_v]:
            n = f(k)
            for g in [rotate90]*4:
                n = tuple(g(n))
                if n not in d:
                    d[n] = d[k]
    return d

def loop_block(dic, line_block):
    d = len(line_block)
    l = []
    # print(line_block, d, len(line_block[0]))
    for i in range(0, len(line_block[0]), d):
        b = []
        for j in range(d):
            b.append(line_block[j][i:i+d])
        # print(b)
        l.append(dic[tuple(b)])
    return ["".join(x) for x in zip(*l)]

def loop_lines(dic, img):
    if len(img) % 2 == 0:
        d = 2
    elif len(img) % 3 == 0:
        d = 3
    else:
        raise(Exception("wrong image"))
    l = []
    for i in range(0, len(img), d):
        l.extend(loop_block(dic, img[i:i+d]))
    return l

def jour21(dic, img, step):
    for _ in range(step):
        img = loop_lines(dic, img)
    return img


start_image = ".#./..#/###".split("/")

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2017 - Jour 21')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    d = create_dict(args.input)
    # pprint(d)
    img = jour21(d, start_image, 5)
    print("Part 1:", sum([x.count('#') for x in img]))
    # print("\n".join(img))

    img = jour21(d, img, 18 - 5)
    print("Part 2:", sum([x.count('#') for x in img]))
    # print("\n".join(img))

    if Image:
        byte_data = [255 * (x == '#') for l in img for x in l]
        im = Image.new('L', (len(img), len(img)))
        im.putdata(byte_data)
        im.save("jour21.png")
