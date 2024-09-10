class Minesweeper
  @annotate: (minefield) ->
    board = minefield.map((row) -> [row...].map (cell) -> if cell == '*' then 99 else 0)

    for _, i in board
      for _, j in board[i]
        if board[i][j] >= 99
          for [di,dj] in [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]]
            if 0 <= i+di < board.length and 0 <= j+dj < board[i].length
              board[i+di][j+dj] += 1

    stringifyCount = (n) ->
      switch 
        when n == 0 then ' '
        when n >= 99 then '*'
        else n

    board.map (row) -> row.map(stringifyCount).join('')

module.exports = Minesweeper
