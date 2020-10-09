from math import `^`

proc onSquare*(n: int): uint64 =
  if n notin 1..64:
    raise newException(ValueError, "square not on the board")
  2'u64 ^ (n - 1)

proc total*(): uint64 =
  (2'u64 ^ 64) - 1
