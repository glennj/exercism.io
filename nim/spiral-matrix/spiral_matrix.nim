import deques

type
  Delta = tuple[x, y: int]

proc initDeltas(): Deque[Delta] =
  result = initDeque[Delta]()
  result.addLast (0,1)
  result.addLast (1,0)
  result.addLast (0,-1)
  result.addLast (-1,0)

proc spiral*(size: static int): array[size, array[size, int]] =
  var deltas = initDeltas()
  proc nextDelta(): Delta =
    deltas.addLast deltas.popFirst
    deltas.peekLast

  var x = 0
  var y = 0
  var next = nextDelta()

  for i in 1 .. size*size:
    result[x][y] = i
    if  x + next.x notin 0..<size or
        y + next.y notin 0..<size or
        result[x + next.x][y + next.y] != 0:
      next = nextDelta()
    x.inc next.x
    y.inc next.y
