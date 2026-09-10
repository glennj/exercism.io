List = require 'pl.List'

get_combinations = (sum, size, exclude) ->
  if size == 1
    return {} if sum < 1 or sum > 9 or exclude\contains sum
    return {List {sum}}

  combos = List!
  for d = 1, 9
    if not exclude\contains d
      for c in *get_combinations(sum - d, size - 1, exclude\append(d))
        combo = c\append(d)\sorted!
        combos\append combo if not combos\contains combo
  combos

{
  combinations: (input) ->
    get_combinations input.sum, input.size, List input.exclude
}
