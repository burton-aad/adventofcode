#!/usr/bin/python

from __future__ import print_function
import re
import sys

class State:
    def __init__(self, name):
        self.name = name
        self.write = [0, 0]
        self.move = [0, 0]
        self.next = [self, self]

    def run(self, tape, pos):
        v = int(pos in tape)
        if self.write[v]:
            tape.add(pos)
        else:
            tape.discard(pos)
        return pos + self.move[v], self.next[v]

    def __repr__(self):
        return "{} : 0 -> ({}, {}, {}), 1 -> ({}, {}, {})".format(self.name, self.write[0], self.move[0], self.next[0].name, self.write[1], self.move[1], self.next[1].name)

class Turing:
    def __init__(self):
        self.states = {}
        self.steps = 0
        self.state = None
        self.tape = set()
        self.pos = 0

    def from_text(self, text):
        for l in text:
            m = re.match(r"\s*- Write the value (\d+).", l)
            if m:
                s.write[v] = int(m.group(1))
                continue
            m = re.match(r"\s*- Move one slot to the (\w+).", l)
            if m:
                if m.group(1) == "right":
                    s.move[v] = 1
                else:
                    s.move[v] = -1
                continue
            m = re.match(r"\s*- Continue with state (\w+).", l)
            if m:
                s.next[v] = self.states.setdefault(m.group(1), State(m.group(1)))
                continue
            m = re.match(r"\s*If the current value is (\d+):", l)
            if m:
                v = int(m.group(1))
                continue
            m = re.match(r"In state (\w+):", l)
            if m:
                s = self.states.setdefault(m.group(1), State(m.group(1)))
                continue
            m = re.match(r"Begin in state (\w)+.", l)
            if m:
                self.state = self.states.setdefault(m.group(1), State(m.group(1)))
                continue
            m = re.match(r"Perform a diagnostic checksum after (\d+) steps.", l)
            if m:
                self.steps = int(m.group(1))
                continue
        return self

    def run(self):
        print("goto {}".format(self.steps))
        for i in range(self.steps):
            if i % 100000 == 0:
                print(i)
            self.pos, self.state = self.state.run(self.tape, self.pos)

    def __repr__(self):
        l = []
        for s in self.states.values():
            l.append(str(s))
        l.sort()
        return "Turing( "+", ".join(l)+" )"


if __name__ == "__main__":
    fname = "input25"
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    with open(fname) as f:
        machine = Turing().from_text(f)
    print(machine)
    machine.run()
    # print(machine.tape)
    print(len(machine.tape))
