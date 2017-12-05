#!/usr/bin/python

import re

class Tree:
    def __init__(self, low, high):
        self.data = []
        self.low_target = low[0]
        self.low = int(low[1])
        self.high_target = high[0]
        self.high = int(high[1])

    def __str__(self):
        return "({left}, {right})".format(**self.__dict__)


def send_output(t, o, cur_bot):
    bot = t[cur_bot]
    #print "send_output", cur_bot, bot.data
    if len(bot.data) == 2:
        low = min(bot.data)
        high = max(bot.data)
        print "bot {} -> {} to {} {}, {} to {} {}".format(
            cur_bot, low, bot.low_target, bot.low, high,
            bot.high_target, bot.high)
        if bot.low_target == "bot":
            t[bot.low].data.append(low)
            if len(t[bot.low].data) == 2:
                send_output(t, o, bot.low)
        else:
            o[bot.low].append(low)
        if bot.high_target == "bot":
            t[bot.high].data.append(high)
            if len(t[bot.high].data) == 2:
                send_output(t, o, bot.high)
        else:
            o[bot.high].append(high)

rules = []
vals = []
with open("input10") as f:
    for l in f:
        if l.startswith("bot "):
            rules.append(l.strip())
        else:
            vals.append(l.strip())

vals.sort(key=lambda x: int(x.split()[-1]))

t = [None] * len(rules)
o = [[] for i in range(len(rules))]
for s in rules:
    s = s.split()
    t[int(s[1])] = Tree(s[5:7], s[10:])

for s in vals:
    print s
    _, value, bot = re.split("[^\d]+", s)
    bot = int(bot)
    t[bot].data.append(int(value))
    send_output(t, o, bot)

# print o
print o[0][0] * o[1][0] * o[2][0]
