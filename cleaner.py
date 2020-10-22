#!/usr/bin/env python3

from pathlib import Path
import os

currDir = os.getcwd()

def gen(a, b):
    yield from a
    yield from b

for path in gen(Path(currDir).rglob('*.hi'), Path('.').rglob('*.o')):
    filePath = os.path.join(currDir, path)
    os.remove(filePath)

os.remove("parser.exe")
os.remove("lexer.hs")
