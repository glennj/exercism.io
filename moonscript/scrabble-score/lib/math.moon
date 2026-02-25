import fold from require 'moon'

-- sum up the elements of a table
sum = (t) ->
  fold {0, table.unpack t}, (sum, n) -> sum + n

{ :sum }
