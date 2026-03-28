import fold from require 'moon'

-- sum up the elements of a table
sum = (t) ->
  fold {0, table.unpack t}, (s, n) -> s + n

product = (t) ->
  fold {1, table.unpack t}, (prod, n) -> prod * n

gcd = (a, b) ->
  b == 0 and a or gcd b, a % b


{ :sum, :product, :gcd }
