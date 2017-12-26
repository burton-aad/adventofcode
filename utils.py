#!/usr/bin/python

from __future__ import print_function


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
