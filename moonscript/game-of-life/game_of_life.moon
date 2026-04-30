{
  tick: (matrix) ->
    return {} if not next matrix

    width = #matrix[1]
    height = #matrix
    result = [ [0 for _ = 1, width] for _ = 1, height]

    neighbourIndices = (i, j) ->
      ns = [{x: i + di, y: j + dj} for di = -1, 1 for dj = -1, 1 when not (di == 0 and dj == 0)]
      [n for n in *ns when n.x >= 1 and n.x <= width and n.y >= 1 and n.y <= height]
    
    -- first count the live neighbors for each cell
    for x = 1, height
      for y = 1, width
        if matrix[x][y] == 1
          for n in *neighbourIndices x, y
            result[n.x][n.y] += 1

    -- then apply the rules of the game
    for x = 1, height
      for y = 1, width
        result[x][y] = switch result[x][y]
          when 3 then 1
          when 2 then matrix[x][y]
          else        0
    result 
}
