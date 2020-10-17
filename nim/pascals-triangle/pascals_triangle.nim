# each cell is the binomial coefficient of its coordinates

from math import binom

proc pascal*(n: Natural): seq[seq[int]] =
  for i in 0..<n:
    result.add newSeq[int](i+1)
    for j in 0..i:
      result[i][j] = binom(i, j)
