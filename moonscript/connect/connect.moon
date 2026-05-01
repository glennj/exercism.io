split = require('pl.stringx').split
clone = require('pl.tablex').deepcopy

transpose = (matrix) ->
  [ [matrix[i][j] for i = 1, #matrix] for j = 1, #matrix[1]]

Deltas = {{-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}}

is_win = (player, mtx) ->
  assert #mtx > 0
  stack = {}
  height = #mtx
  width = #mtx[1]

  -- a couple of little closures
  inrange = (n, limit) -> 1 <= n and n <= limit
  is_neighbour = (r, c) ->
    inrange(r, height) and inrange(c, width) and mtx[r][c] == player
  neighbours = (r, c) ->
    [{r + dr, c + dc} for {dr, dc} in *Deltas when is_neighbour r + dr, c + dc]

  -- when we add coordinates to the stack, we mark them as "visited"
  enqueue = (r, c) ->
    table.insert stack, {r, c}
    mtx[r][c] = 'seen'

  -- find the players in the top row
  for j = 1, width
    if mtx[1][j] == player
      enqueue 1, j

  return false if #stack == 0
  return true if height == 1

  -- follow friendly neighbours until we reach the bottom,
  -- or we run out of neighbours
  while #stack > 0
    {i, j} = table.remove stack
    for {ii, jj} in *neighbours i, j
      return true if ii == height
      enqueue ii, jj
  false

{
  winner: (board) ->
    matrix = [split row for row in *board]
    return 'O' if is_win 'O', clone matrix
    return 'X' if is_win 'X', transpose matrix
    return ''   
}
