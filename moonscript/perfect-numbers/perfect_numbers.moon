aliquotSum = (number) ->
  sum = 0
  limit = math.floor math.sqrt number
  for f = 1, limit
    g = number // f
    if f * g == number
      sum += f
      sum += g if f != g
  sum - number

{
  classify: (number) ->
    assert number > 0, 'Classification is only possible for positive integers.'
    sum = aliquotSum number
    if sum < number
      'deficient'
    elseif sum > number
      'abundant'
    else
      'perfect'
}
