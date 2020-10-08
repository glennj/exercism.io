from math import hypot

type Position = tuple
  x, y: float

proc score*(p: Position): int =
  var dist = hypot(p.x, p.y)
  if   dist <=  1: 10
  elif dist <=  5:  5
  elif dist <= 10:  1
  else:             0
    
