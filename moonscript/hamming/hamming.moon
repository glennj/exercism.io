distance = (left, right) ->
  assert #left == #right, 'strands must be of equal length'

  #[i for i = 1, #left when left\sub(i, i) != right\sub(i, i)]

{ :distance }
