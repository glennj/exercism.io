class FlowerField
  @annotate: (flowerfield) ->
    field = flowerfield.map((row) -> [row...].map (cell) -> if cell == '*' then 99 else 0)

    for _, i in field
      for _, j in field[i]
        if field[i][j] >= 99
          for [di,dj] in [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]]
            if 0 <= i+di < field.length and 0 <= j+dj < field[i].length
              field[i+di][j+dj] += 1

    stringifyCount = (n) ->
      switch 
        when n == 0 then ' '
        when n >= 99 then '*'
        else n

    field.map (row) -> row.map(stringifyCount).join('')

module.exports = FlowerField
