use context starter2024

include file("saddle-points.arr")

check "Can identify single saddle point":
  matrix = [list:
    [list: 9, 8, 7],
    [list: 5, 3, 2],
    [list: 6, 6, 7]]
  expected = [list: tree(2, 1)]

  saddle-points(matrix) is expected
end

check "Can identify that empty matrix has no saddle points":
  matrix = [list: ]
  expected = [list: ]

  saddle-points(matrix) is expected
end

check "Can identify lack of saddle points when there are none":
  matrix = [list:
    [list: 1, 2, 3],
    [list: 3, 1, 2],
    [list: 2, 3, 1]]
  expected = [list: ]

  saddle-points(matrix) is expected
end

check "Can identify multiple saddle points in a column":
  matrix = [list:
    [list: 4, 5, 4],
    [list: 3, 5, 5],
    [list: 1, 5, 4]]
  expected = [list:
    tree(1, 2),
    tree(2, 2),
    tree(3, 2)]

  saddle-points(matrix) is expected
end

check "Can identify multiple saddle points in a row":
  matrix = [list:
    [list: 6, 7, 8],
    [list: 5, 5, 5],
    [list: 7, 5, 6]]
  expected = [list:
    tree(2, 1),
    tree(2, 2),
    tree(2, 3)]

  saddle-points(matrix) is expected
end

check "Can identify saddle point in bottom right corner":
  matrix = [list:
    [list: 8, 7, 9],
    [list: 6, 7, 6],
    [list: 3, 2, 5]]
  expected = [list: tree(3, 3)]

  saddle-points(matrix) is expected
end

check "Can identify saddle points in a non square matrix":
  matrix = [list:
    [list: 3, 1, 3],
    [list: 3, 2, 4]]
  expected = [list:
    tree(1, 1),
    tree(1, 3)]

  saddle-points(matrix) is expected
end

check "Can identify that saddle points in a single column matrix are those with the minimum value":
  matrix = [list:
    [list: 2],
    [list: 1],
    [list: 4],
    [list: 1]]
  expected = [list:
    tree(2, 1),
    tree(4, 1)]

  saddle-points(matrix) is expected
end

check "Can identify that saddle points in a single row matrix are those with the maximum value":
  matrix = [list: [list: 2, 5, 3, 5]]
  expected = [list:
    tree(1, 2),
    tree(1, 4)]

  saddle-points(matrix) is expected
end
