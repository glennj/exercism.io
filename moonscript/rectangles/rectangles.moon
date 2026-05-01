-- Using classes here is overkill, but it's a tidy way to encapsulate the functionality.
-- ---------------------------------------------
class Matrix
  new: (input) =>
    @m = [ [c for c in row\gmatch '.'] for row in *input]

  height: => #@m
  width: => #@m[1]

  isVertex: (row, col) =>
    @m[row][col] == '+'

  isHorizontalLine: (row, i, j) =>
    for k = i, j
      return false if not (@m[row][k] == '-' or @m[row][k] == '+')
    true

  isVerticalLine: (col, i, j) =>
    for k = i, j
      return false if not (@m[k][col] == '|' or @m[k][col] == '+')
    true

  isRectangle: (topRight, bottomLeft) =>
    return false unless @isVertex bottomLeft.row, topRight.col
    return false unless @isHorizontalLine topRight.row, bottomLeft.col, topRight.col
    return false unless @isHorizontalLine bottomLeft.row, bottomLeft.col, topRight.col
    return false unless @isVerticalLine bottomLeft.col, topRight.row, bottomLeft.row
    return false unless @isVerticalLine topRight.col, topRight.row, bottomLeft.row
    true

-- ---------------------------------------------
-- Mix and match MoonScript and Lua OO style,
-- all to make `for v in *vertices` look like normal MoonScript looping
Vertices = {}
Vertices.__index = Vertices

Vertices.fromMatrix = (matrix) ->
  vs = [{row: r, col: c} for r = 1, matrix\height! for c = 1, matrix\width! when matrix\isVertex r, c]
  setmetatable vs, Vertices
  vs

Vertices.rightOf = (vertex) =>
  [v for v in *self when v.row == vertex.row and v.col > vertex.col]

Vertices.below = (vertex) =>
  [v for v in *self when v.row > vertex.row and v.col == vertex.col]

-- ---------------------------------------------
{
  rectangles: (input) ->
    matrix = Matrix input
    vertices = Vertices.fromMatrix matrix
    count = 0
    for topLeft in *vertices
      for topRight in *vertices\rightOf topLeft
        for bottomLeft in *vertices\below topLeft
          -- a rectangle can be defined by 2 opposite vertices
          if matrix\isRectangle topRight, bottomLeft
            count += 1
    count
}
