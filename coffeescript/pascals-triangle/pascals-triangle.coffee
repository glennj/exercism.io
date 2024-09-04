class PascalsTriangle
  rows: (num) ->
    @triangle = []
    @row i for i in [0...num]

  row: (i) ->
    @triangle[i] = []
    @cell i, j for j in [0..i]

  cell: (i, j) ->
    @triangle[i][j] =
      switch j
        when 0, i then 1
        else @triangle[i-1][j-1] + @triangle[i-1][j]


module.exports = PascalsTriangle
