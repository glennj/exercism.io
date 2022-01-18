const Size = 8    # board is Size x Size

type
  Queen* = ref object
    row*: int
    col*: int


proc initQueen*(row: int, col: int): Queen =
  if row < 0:
    raise newException(ValueError, "queen must have positive row")
  if col < 0:
    raise newException(ValueError, "queen must have positive column")
  if row >= Size:
    raise newException(ValueError, "queen must have row on board")
  if col >= Size:
    raise newException(ValueError, "queen must have column on board")
  Queen(row: row, col: col)


proc `==`*(q1: Queen, q2: Queen): bool =
  q1.row == q2.row and q1.col == q2.col


proc checkPositions(q1: Queen, q2: Queen): void =
  if q1 == q2:
    raise newException(ValueError, "Queens cannot occupy same position")


proc canAttack*(q1: Queen, q2: Queen): bool =
  checkPositions q1, q2
  q1.row == q2.row or
  q1.col == q2.col or
  (q1.row - q2.row).abs == (q1.col - q2.col).abs


proc cell(x:int, y:int, q1: Queen, q2: Queen): char =
  var pos = Queen(row: x, col: y)
  if pos == q1: return 'W'
  if pos == q2: return 'B'
  return '_'


proc board*(q1: Queen, q2: Queen): string =
  checkPositions q1, q2
  for x in 0 ..< Size:
    for y in 0 ..< Size:
      result &= cell(x, y, q1, q2)
    result &= "\n"
