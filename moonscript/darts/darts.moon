score = (x, y) ->
  distance = math.sqrt(x * x + y * y)
  return 10 if distance <= 1
  return  5 if distance <= 5
  return  1 if distance <= 10
  return  0

{ :score }
