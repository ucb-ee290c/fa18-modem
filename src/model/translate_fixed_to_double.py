#!/usr/bin/env python

import sys


def convert(thestr, width, bp):
    i = int(thestr)
    if i >> width-1:
        i = -(2**width - i)
    return float(i) / 2.0**bp

width=16
bp = 13
translate = ["divider", ">>*", "inverter", "polar"]

for arg in sys.argv[1:]:
    print(str(float(arg) / 2.0**bp))

for ln in sys.stdin:
    ln = ln.rstrip()
    vec = ln.split()
    if any(val in ln for val in translate):
        if ">>*" in ln:
            vec[1] = str(convert(vec[1], width, bp))
        else:
            vec[2] = str(convert(vec[2], width, bp))
        ln = ' '.join(vec)
    print(ln)

