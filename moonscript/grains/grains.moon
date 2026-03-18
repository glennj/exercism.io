import fold from require 'moon'

SIZE = 64

square = (n) ->
  assert 1 <= n and n <= SIZE, 'square must be between 1 and #{SIZE}'
  2 ^ (n - 1)
  
total = ->
  -- the efficient solution:  (2 ^ SIZE) - 1
  -- playing with moony features:
  fold [square i for i = 1, SIZE], (sum, grains) -> sum + grains

{ :square, :total }
