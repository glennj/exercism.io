use context starter2024

include file("pascals-triangle.arr")

check "zero rows":
  rows(0) is [list: ]
end

check "single row":
  expected = [list:
    [list: 1]]

  rows(1) is expected
end

check "two rows":
  expected = [list:
    [list: 1],
    [list: 1, 1]]

  rows(2) is expected
end

check "three rows":
  expected = [list:
    [list: 1],
    [list: 1, 1],
    [list: 1, 2, 1]]

  rows(3) is expected
end

check "four rows":
  expected = [list:
    [list: 1],
    [list: 1, 1],
    [list: 1, 2, 1],
    [list: 1, 3, 3, 1]]

  rows(4) is expected
end

check "five rows":
  expected = [list:
    [list: 1],
    [list: 1, 1],
    [list: 1, 2, 1],
    [list: 1, 3, 3, 1],
    [list: 1, 4, 6, 4, 1]]

  rows(5) is expected
end

check "six rows":
  expected = [list:
    [list: 1],
    [list: 1, 1],
    [list: 1, 2, 1],
    [list: 1, 3, 3, 1],
    [list: 1, 4, 6, 4, 1],
    [list: 1, 5, 10, 10, 5, 1]]

  rows(6) is expected
end

check "ten rows":
  expected = [list:
    [list: 1],
    [list: 1, 1],
    [list: 1, 2, 1],
    [list: 1, 3, 3, 1],
    [list: 1, 4, 6, 4, 1],
    [list: 1, 5, 10, 10, 5, 1],
    [list: 1, 6, 15, 20, 15, 6, 1],
    [list: 1, 7, 21, 35, 35, 21, 7, 1],
    [list: 1, 8, 28, 56, 70, 56, 28, 8, 1],
    [list: 1, 9, 36, 84, 126, 126, 84, 36, 9, 1]]

  rows(10) is expected
end
