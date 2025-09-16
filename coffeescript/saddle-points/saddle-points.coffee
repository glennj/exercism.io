SaddlePoints = (matrix) ->
  saddlePoints = []

  rowMaxima = (Math.max row... for row in matrix)
  colMinima = (Math.min (matrix[i][j] for _, i in matrix)... for _, j in matrix[0])

  for row, i in matrix
    for val, j in row
      if val == rowMaxima[i] && val == colMinima[j]
        saddlePoints.push {row: i + 1, column: j + 1}

  saddlePoints

module.exports = SaddlePoints
