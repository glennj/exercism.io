-- Find all factor pairs of n where each factor is bounded between min and
-- max.
-- For example, the factor pairs of 12 are {{1, 12}, {2, 6}, {3, 4}}
-- but bounded_factors(12, 2, 5) returns {{3, 4}} because that's the only
-- pair where both factors are in the range 2 to 5 (inclusive)
bounded_factors = (n, min, max) ->
  fs = {}
  for f = min, math.min(max, math.sqrt n)
    if n % f == 0
      g = n // f
      if g <= max
        table.insert fs, {f, g}
  fs

-- determine if a number is a palindrome, without converting to string
is_palindrome = (num) ->
  n = num
  p = 0
  while n > 0
    p = p * 10 + n % 10
    n = n // 10
  p == num


seek = (min, max, start, stop, step) ->
  assert min <= max, 'min must be <= max'
  for n = start, stop, step
    if is_palindrome n
      fs = bounded_factors(n, min, max)
      if #fs > 0
        return n, fs
  return nil, {}


{
  smallest: (min, max) ->
    seek min, max, min * min, max * max, 1

  largest: (min, max) ->
    seek min, max, max * max, min * min, -1
}
