from algorithm import reversed
from strutils  import join, repeat


proc diamond*(letter: char): string =
  var size = letter.ord - 'A'.ord + 1
  var rows: seq[string]

  # construct the top half
  for i in 0..<size:
    var halfRow = ' '.repeat(size)
    halfRow[i] = ('A'.ord + i).chr
    rows.add(halfRow[1..^1].reversed.join("") & halfRow & "\n")

  # and the bottom half
  rows.add(rows[0..^2].reversed)

  rows.join("")
