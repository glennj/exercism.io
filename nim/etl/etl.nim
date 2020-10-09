import tables
from strutils import toLowerAscii

proc transform*(input: Table[int, seq[char]]): Table[char, int] =
  for val, chars in input:
    for ch in chars:
      result[ch.toLowerAscii] = val
