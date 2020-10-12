import math, re, strutils

proc encrypt*(plaintext: string): string =
  var cleaned = plaintext.replace(re"[^[:alnum:]]", "").toLowerAscii
  var numGroups = cleaned.len.toFloat.sqrt.ceil.toInt
  var groupSize = (cleaned.len / numGroups).ceil.toInt
  var padded = cleaned.alignLeft(groupSize * numGroups)
  var groups = newSeq[string](numGroups)

  for i, c in padded.pairs:
    groups[i mod numGroups].add c
    
  groups.join(" ")
