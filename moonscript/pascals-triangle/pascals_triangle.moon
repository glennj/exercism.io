{
  rows: (n) ->
    triangle = {}
    for i = 1, n
      row = [(j == 1 or j == i) and 1 or (triangle[i-1][j-1] + triangle[i-1][j]) for j = 1, i]
      table.insert triangle, row
    triangle
}
