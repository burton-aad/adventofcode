#!/usr/bin/env python3

import sys
from collections import deque

class LList:
    class LNode:
        def __init__(self, value):
            self.v = value
            self.next = None

        def __repr__(self):
            if self.next:
                return f"Node({self.v} -> {self.next.v})"
            else:
                return f"Node({self.v} -> {self.next})"

    def __init__(self, inputs, max_elem = None):
        max_input = max(inputs)
        if max_elem is None:
            max_elem = max_input
        self.max_elem = max_elem
        self.nodes = [LList.LNode(i+1) for i in range(max_elem)]
        for n in self.nodes:
            n.next = self.nodes[(n.v) % max_elem]
        for c, n in zip([self.nodes[-1].v] + inputs, inputs):
            self.nodes[c-1].next = self.nodes[n-1]
        if max_elem > max_input:
            self.nodes[inputs[-1]-1].next = self.nodes[max_input]
        else:
            self.nodes[inputs[-1]-1].next = self.nodes[inputs[0]-1]
        self.head = self.nodes[inputs[0]-1]

    def pop_after(self, n):
        r = n.next
        n.next, r.next = r.next, None
        return r

    def push_after(self, value, new_n):
        n = self.nodes[value-1]
        n.next, new_n.next = new_n, n.next

    def __getitem__(self, i):
        return self.nodes[i]

    def iter_from(self, n):
        return self.__iter__(n.v-1)

    def __iter__(self, head=0):
        n = self.nodes[head]
        while n.next != self.nodes[head]:
            yield n
            n = n.next
        yield n

    def prepr(self, head=None):
        return " ".join(str(n.v) for n in self.iter_from(head if head else self.head))

def target(cur, l, max_cup):
    t = cur - 1
    if t == 0:
        t = max_cup
    while t in l:
        t -= 1
        if t == 0:
            t = max_cup
    return t

def move(d, cur):
    b = [d.pop_after(cur), d.pop_after(cur), d.pop_after(cur)]
    dest = target(cur.v, [n.v for n in b], d.max_elem)
    for n in b[::-1]:
        d.push_after(dest, n)
    return cur.next

def Jour23(p):
    l = LList([int(i) for i in str(p)])
    cur = l.head
    for _ in range(100):
        cur = move(l, cur)
    print("Part 1: ", "".join(map(lambda n: str(n.v), l.iter_from(l[0].next)))[:-1])

def Jour23_2(p):
    l = LList([int(i) for i in str(p)], 1_000_000)
    cur = l.head
    for _ in range(10_000_000):
        cur = move(l, cur)
    print("Part 2: ", l[0].next.v * l[0].next.next.v)

if __name__=="__main__":
    # input = 389125467
    input = 562893147
    Jour23(input)
    Jour23_2(input) # ~30sec to make all moves
