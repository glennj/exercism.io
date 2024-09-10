class SpiralMatrix
  @spiralMatrix: (size) ->
    return [] if size == 0

    matrix = Array.from({length: size}, -> new Array(size))
    [i, j, di, dj] = [0, 0, 0, 1]

    endOfRow = -> j + dj == size or j + dj < 0 or i + di == size or matrix[i+di][j+dj]?

    for n in [1..size ** 2]
      matrix[i][j] = n
      [di, dj] = [dj, -di] if endOfRow()
      i += di
      j += dj

    matrix


module.exports = SpiralMatrix

