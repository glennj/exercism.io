{
  saddle_points: (mtx) ->
    row_max = [math.mininteger for _ = 1, #mtx]
    col_min = [math.maxinteger for _ = 1, #mtx[1]]

    for i = 1, #mtx
      for j = 1, #mtx[i]
        row_max[i] = math.max mtx[i][j], row_max[i]
        col_min[j] = math.min mtx[i][j], col_min[j]

    saddle_points = {}
    for i = 1, #mtx
      for j = 1, #mtx[i]
        if mtx[i][j] == row_max[i] and mtx[i][j] == col_min[j]
          table.insert saddle_points, {row: i, column: j}

    saddle_points
}
