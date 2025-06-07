class GameOfLife
  constructor: (@matrix) ->
    @rows = @matrix.length
    @cols = if @rows is 0 then undefined else @matrix[0].length


  tick: () ->
    return if @rows is 0

    newMatrix = (new Array(@cols).fill(0) for [0...@rows])

    for row in [0...@rows]
      for col in [0...@cols]
        newMatrix[row][col] = switch @countNeighbours(row, col)
          when 3 then 1                  # becomes live
          when 2 then @matrix[row][col]  # stays live if live
          else 0                         # dies

    @matrix = newMatrix


  countNeighbours: (row, col) ->
    count = 0

    for delta in [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]]
      dr = row + delta[0]
      dc = col + delta[1]
      if 0 <= dr < @rows and 0 <= dc < @cols
        count = count + @matrix[dr][dc]

    count


module.exports = GameOfLife
