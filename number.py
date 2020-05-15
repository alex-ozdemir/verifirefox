#!/usr/bin/env python3

import fileinput
import re
import sys

ctr = 0
for line in sys.stdin.readlines():
    if re.search("BEGIN VERIFIREFOX FAIL",line):
        ctr = 0
    r = re.compile(f"({sys.argv[1]})")
    if re.search(sys.argv[1],line):
        print(r.sub(f"\\1:{ctr}", line), end="")
        ctr+=1
    else:
        print(line, end="")
