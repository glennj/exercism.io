use context starter2024

include file("game-of-life.arr")

check "empty matrix":
  matrix = [list: ]
  expected = [list: ]

  tick(matrix) is expected
end

check "live cells with zero live neighbors die":
  matrix = [list:
    [list: 0, 0, 0],
    [list: 0, 1, 0],
    [list: 0, 0, 0]]
  expected = [list:
    [list: 0, 0, 0],
    [list: 0, 0, 0],
    [list: 0, 0, 0]]

  tick(matrix) is expected
end

check "live cells with only one live neighbor die":
  matrix = [list:
    [list: 0, 0, 0],
    [list: 0, 1, 0],
    [list: 0, 1, 0]]
  expected = [list:
    [list: 0, 0, 0],
    [list: 0, 0, 0],
    [list: 0, 0, 0]]

  tick(matrix) is expected
end

check "live cells with two live neighbors stay alive":
  matrix = [list:
    [list: 1, 0, 1],
    [list: 1, 0, 1],
    [list: 1, 0, 1]]
  expected = [list:
    [list: 0, 0, 0],
    [list: 1, 0, 1],
    [list: 0, 0, 0]]

  tick(matrix) is expected
end

check "live cells with three live neighbors stay alive":
  matrix = [list:
    [list: 0, 1, 0],
    [list: 1, 0, 0],
    [list: 1, 1, 0]]
  expected = [list:
    [list: 0, 0, 0],
    [list: 1, 0, 0],
    [list: 1, 1, 0]]

  tick(matrix) is expected
end

check "dead cells with three live neighbors become alive":
  matrix = [list:
    [list: 1, 1, 0],
    [list: 0, 0, 0],
    [list: 1, 0, 0]]
  expected = [list:
    [list: 0, 0, 0],
    [list: 1, 1, 0],
    [list: 0, 0, 0]]

  tick(matrix) is expected
end

check "live cells with four or more neighbors die":
  matrix = [list:
    [list: 1, 1, 1],
    [list: 1, 1, 1],
    [list: 1, 1, 1]]
  expected = [list:
    [list: 1, 0, 1],
    [list: 0, 0, 0],
    [list: 1, 0, 1]]

  tick(matrix) is expected
end

check "bigger matrix":
  matrix = [list:
    [list: 1, 1, 0, 1, 1, 0, 0, 0],
    [list: 1, 0, 1, 1, 0, 0, 0, 0],
    [list: 1, 1, 1, 0, 0, 1, 1, 1],
    [list: 0, 0, 0, 0, 0, 1, 1, 0],
    [list: 1, 0, 0, 0, 1, 1, 0, 0],
    [list: 1, 1, 0, 0, 0, 1, 1, 1],
    [list: 0, 0, 1, 0, 1, 0, 0, 1],
    [list: 1, 0, 0, 0, 0, 0, 1, 1]]
  expected = [list:
    [list: 1, 1, 0, 1, 1, 0, 0, 0],
    [list: 0, 0, 0, 0, 0, 1, 1, 0],
    [list: 1, 0, 1, 1, 1, 1, 0, 1],
    [list: 1, 0, 0, 0, 0, 0, 0, 1],
    [list: 1, 1, 0, 0, 1, 0, 0, 1],
    [list: 1, 1, 0, 1, 0, 0, 0, 1],
    [list: 1, 0, 0, 0, 0, 0, 0, 0],
    [list: 0, 0, 0, 0, 0, 0, 1, 1]]

  tick(matrix) is expected
end
