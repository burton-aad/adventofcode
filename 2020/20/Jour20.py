#!/usr/bin/env python3

import sys, os
import itertools
import functools
import math
import re
import argparse

from utils import sliding_window


def hflip(pix):
    return ["".join(reversed(p)) for p in pix]

def vflip(pix):
    return ["".join(p) for p in pix[::-1]]

def rotate(pix):
    return hflip(zip(*pix))

def rotateleft(pix):
    return vflip(list(zip(*pix)))

class Tile:
    def __init__(self, id, f):
        self.id = id
        self.pix = []
        for l in f:
            if l.strip() == "":
                break
            self.pix.append(l.strip())

    @property
    def edges(self):
        to_int = lambda s: int("".join("0" if c == "." else "1" for c in s), 2)
        return (
            frozenset((to_int(self.pix[0]), to_int(reversed(self.pix[0])))),
            frozenset((to_int(self.pix[-1]), to_int(reversed(self.pix[-1])))),
            frozenset((to_int(p[0] for p in self.pix), to_int(p[0] for p in self.pix[::-1]))),
            frozenset((to_int(p[-1] for p in self.pix), to_int(p[-1] for p in self.pix[::-1]))),
        )

    def hflip(self):
        self.pix = hflip(self.pix)

    def vflip(self):
        self.pix = vflip(self.pix)

    def rotate(self):
        self.pix = rotate(self.pix)

    def rotateleft(self):
        self.pix = rotateleft(self.pix)

    def __repr__(self):
        return "Tile({})".format(self.id)

    def __str__(self):
        return "\n".join(self.pix)


class TileImage:
    def __init__(self, corner_tile, size):
        self.size = size
        self.img = [[None] * size for _ in range(size)]
        self.img[0][0] = corner_tile
        self._fill_tile_image()
        self._fix_tiles_orientations()

    def _fill_tile_image(self):
        # fill image from left to right, up to bottom
        # this way as the neighbor are send as up left down right, there is no
        # ambiguity on the position of each neighbor
        for i, img_l in enumerate(self.img):
            for j, tile in enumerate(img_l):
                if tile is None:
                    raise RuntimeError("tile should not be None in the loop")
                tile_set = {t for l in self.img for t in l}
                l = [lk for lk in tile.links if lk not in tile_set]
                for vi, vj in self.valid4neighbor(i, j):
                    for lk in l:
                        if self._isvalid(lk, vi, vj):
                            self.img[vi][vj] = lk
                            l.remove(lk)
                            break
                if l:
                    # at the end of the loop, all links shall be placed
                    raise RuntimeError("Unhandled links ", l)

    def _fix_tiles_orientations(self):
        # get correct tiles orientation
        no_act = lambda: None
        for i, j in itertools.product(range(self.size), repeat=2):
            tile = self.img[i][j]
            # edges are top, bottom, left, right
            searches = []
            if j < self.size-1: # search for right
                searches.append([(i, j+1), [tile.rotate, tile.rotateleft, tile.hflip, no_act]])
            if i < self.size-1: # search for bottom
                searches.append([(i+1, j), [tile.vflip, no_act, tile.rotateleft, tile.rotate]])
            if j > 0: # search for left
                searches.append([(i, j-1), [tile.rotateleft, tile.rotate, no_act, tile.hflip]])
            if i > 0: # search for up
                searches.append([(i-1, j), [no_act, tile.vflip, tile.rotate, tile.rotateleft]])
            for vois, actions in searches:
                for e, act in zip(tile.edges, actions):
                    if e in self.get(*vois).edges:
                        act()
                        break

    def __getitem__(self, i):
        return self.img[i]

    def __repr__(self):
        return repr(self.img)

    def get(self, i, j):
        return self.img[i][j]

    def print_tiles(self):
        for l in self.img:
            for i in range(len(l[0].pix)):
                print(" ".join(t.pix[i] for t in l))
            print()

    def _isvalid(self, tile, i, j):
        # check the tile can be placed at position (i, j)
        if self.img[i][j] is not None:
            return tile.id == self.img[i][j].id
        return all(self.img[vi][vj] is None
                   or self.img[vi][vj] in tile.links
                   for vi, vj in self.valid4neighbor(i, j))

    def valid4neighbor(self, i, j):
        # return neighbors if valid
        r = []
        if i > 0: r.append((i-1, j)) # up
        if j > 0: r.append((i, j-1)) # left
        if i < self.size-1: r.append((i+1, j)) # down
        if j < self.size-1: r.append((i, j+1)) # right
        return r


def make_image(tile_image):
    image = []
    tile_size = len(tile_image[0][0].pix)
    for tline in tile_image.img:
        for i in range(1, tile_size-1):
            image.append("".join(t.pix[i][1:tile_size-1] for t in tline))
    return image

def find_sea_monster(image, sea_monster):
    regex = [re.compile("(?=(" + l.replace(" ", ".") + "))") for l in sea_monster]
    m_idx = []
    no_act = lambda x: x
    for trans in [no_act, rotate, rotate, rotate]:
        image = trans(image)
        for int_trans in [no_act, hflip, vflip]:
            image = int_trans(image)
            for i, lines in enumerate(sliding_window(image, 3)):
                for m in regex[0].finditer(lines[0]):
                    idx = m.start()
                    if regex[1].match(lines[1][idx:]) and regex[2].match(lines[2][idx:]):
                        m_idx.append((i, idx))
            image = int_trans(image)
            if m_idx:
                return m_idx
    return m_idx


if __name__=="__main__":
    parser = argparse.ArgumentParser(description='AoC 2020 - Jour 20')
    parser.add_argument("input", nargs='?', default="input")
    args = parser.parse_args()

    with open(args.input) as f:
        tiles = [Tile(int(l.split()[1][:-1]), f) for l in f if l.strip()]

    # on suppose que l'input est sympa et qu'il n'y a pas de multiple possibilité
    for t in tiles:
        t.links = [ti for ti in tiles if ti.id != t.id and len(set(t.edges) & set(ti.edges)) > 0]

    corners = [t for t in tiles if len(t.links) == 2]
    print("Part 1: ", functools.reduce(lambda a, b: a*b.id, corners, 1))

    tile_image = TileImage(corners[0], int(math.sqrt(len(tiles))))
    # tile_image.print_tiles()
    real_image = make_image(tile_image)
    # print("\n".join(real_image))

    sea_monster = ["                  # ",
                   "#    ##    ##    ###",
                   " #  #  #  #  #  #   "]
    m_idx = find_sea_monster(real_image, sea_monster)
    print("Part 2: ", sum(l.count("#") for l in real_image) - sum(l.count("#") for l in sea_monster) * len(m_idx))
