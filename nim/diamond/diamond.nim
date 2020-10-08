from algorithm import reversed
from strutils  import join, repeat
from sequtils  import toSeq

proc diamond*(letter: char): string =
  var letters = ('A'..letter).toSeq
  var rows: seq[string]

  # construct the top half
  for i in 0..<letters.len:
    var halfRow = ' '.repeat(letters.len)
    halfRow[i] = letters[i]
    rows.add(halfRow[1..^1].reversed.join("") & halfRow & "\n")

  # and the bottom half
  rows.add(rows[0..^2].reversed)

  rows.join("")
