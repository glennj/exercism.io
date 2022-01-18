import algorithm

proc latest*(scores: seq[int]): int = scores[^1]

proc personalBest*(scores: seq[int]): int = scores.max

proc personalTopThree*(scores: seq[int]): seq[int] =
  var scoresSorted = scores.sorted.reversed
  try:
    for i in 0..2:
      result.add scoresSorted[i]
  except IndexError:
    discard

