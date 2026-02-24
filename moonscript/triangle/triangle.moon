valid = (...) ->
  sides = {...}
  table.sort sides
  {a, b, c} = sides
  a > 0 and a + b > c


is_equilateral = (a, b, c) ->
  valid(a, b, c) and a == b and b == c

is_isosceles = (a, b, c) ->
  valid(a, b, c) and (a == b or b == c or c == a)

is_scalene = (a, b, c) ->
  valid(a, b, c) and not (a == b or b == c or c == a)

{ :is_equilateral, :is_isosceles, :is_scalene }
