local ident, square, sum

square_of_sum = (n) ->
  -- boring: return ((n * (n + 1)) / 2) ^ 2
  -- fun: functional
  sum n, ident, square

sum_of_squares = (n) ->
  -- boring: return n * (n + 1) * (2 * n + 1) / 6
  -- fun: functional
  sum n, square, ident

difference_of_squares = (n) ->
  square_of_sum(n) - sum_of_squares(n)

-- ------------------------------------------------------------
ident = (x) -> x
square = (x) -> x * x

sum = (n, inner, outer) ->
  result = 0
  for i = 1, n
    result += inner i
  outer result

-- ------------------------------------------------------------
{ :square_of_sum, :sum_of_squares, :difference_of_squares }
