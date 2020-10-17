import sequtils

type
  Matrix = seq[seq[int]]
  Cell   = (int, int)

proc isEmpty(matrix: Matrix): bool = matrix.len == 0

proc transpose(matrix: Matrix): Matrix =
  for i in 0 ..< matrix[0].len:
    result.add matrix.mapIt(it[i])

proc saddlePoints*(matrix: Matrix): seq[Cell] =
  if not matrix.isEmpty:
    for x, rowMax in matrix.mapIt(it.max).pairs:
      for y, colMin in matrix.transpose.mapIt(it.min).pairs:
        if matrix[x][y] == rowMax and matrix[x][y] == colMin:
          result.add (x+1, y+1)
