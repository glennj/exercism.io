import strutils, sequtils

proc isIsogram*(input: string): bool =
  var chars: set[char]

  for c in input.toLowerAscii.filter(isAlphaAscii):
    if (c in chars): return false
    chars.incl(c)

  return true
