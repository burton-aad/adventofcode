#!/usr/bin/python

from __future__ import print_function
from itertools import zip_longest, tee, filterfalse


infinity = 1000000000 # 1000000000 est (espérons le) l'infini

def min_dist(D, Q):
    d = infinity
    v = -1
    for s in Q:
        if D[s] < d:
            v = s
            d = D[s]
    return v

def dijkstra(G, s):
    """
    Alghorithme de Dijkstra implémenté pour un certain type de graphe simplifié.
    Forme du graphe : un dictionnaire de lien avec poids
    G = {0: {1:1}, 1: {0:1, 2:7, 3:5}, 2: {1:7, 3:1}, 3: {1:5, 2:1}}
    -->      1        7
        "0" --- "1" ----- "2"
                 |         |
                 |         |
                5|-- "3" --|1
    Retourne la plus courte distance du point s aux autres points et le tableau des précédences :
    dijkstra(G, 0)
    --> ( {0: 0, 1: 1, 2: 6, 3: 5}, {1: 0, 2: 3, 3: 1} )
    """
    if s not in G:
        raise(Exception("s not in G"))
    D = dict([(k, infinity) for k in G])
    D[s] = 0
    P = {} # les noeuds non reliés à s n'apparaitront pas dans P
    Q = set(G.keys())
    # print(Q)
    # print(D)
    while len(Q) > 0:
        r = min_dist(D, Q)
        if r == -1:
            break
        Q.remove(r)
        # print(r, "-->", Q)
        voisins = set(G[r])
        for v in voisins.intersection(Q):
            if D[v] > D[r] + G[r][v]:
                D[v] = D[r] + G[r][v]
                P[v] = r
        # print(D)
    return D, P

def grouper(iterable, n, fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

def sliding_window(iterable, n):
    "Get data through a sliding window of size n"
    # sliding_window('ABCDEFG', 3) --> ABC BCD CDE DEF EFG
    b, r = iterable, []
    for _ in range(n):
        a, b = tee(b)
        next(b, None)
        r.append(a)
    return zip(*r)

def partition(pred, iterable):
    "Use a predicate to partition entries into false entries and true entries"
    # partition(is_odd, range(10)) --> 1 3 5 7 9  and  0 2 4 6 8
    t1, t2 = tee(iterable)
    return filter(pred, t1), filterfalse(pred, t2)
