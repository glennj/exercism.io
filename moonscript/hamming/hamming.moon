distance = (left, right) ->
  assert #left == #right, 'strands must be of equal length'

  dist = 0
  for i = 1, #left
    dist += 1 if left\sub(i, i) ~= right\sub(i, i)
  dist

{ :distance }
