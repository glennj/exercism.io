const Size = 8    # board is Size x Size

type
  Position = tuple
    x, y: int


proc validate(p: Position): void =
  if  p.x < 0 or p.x >= Size or 
      p.y < 0 or p.y >= Size:
    raise newException(ValueError, "Invalid position")


proc validate(white: Position, black: Position): void =
  validate white
  validate black
  if white == black:
    raise newException(ValueError, "Queens cannot occupy same position")


proc canAttack*(white: Position, black: Position): bool =
  validate white, black
  white.x == black.x or
  white.y == black.y or
  (white.x - black.x).abs == (white.y - black.y).abs


proc cell(p: Position, white: Position, black: Position): char =
  if p == white: return 'W'
  if p == black: return 'B'
  return '_'

proc board*(white: Position, black: Position): seq[string] =
  validate white, black
  for x in 0 ..< Size:
    var row = ""
    for y in 0 ..< Size:
      row.add cell((x, y), white, black)
    result.add row

