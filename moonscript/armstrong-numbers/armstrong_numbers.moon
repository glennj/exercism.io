armstrong_sum = (num) ->
  width = 1 + math.floor(math.log num, 10)
  sum = 0
  while num > 0
    sum += (num % 10) ^ width
    num = num // 10    -- there is no `//=` updating assignment
  sum

  
is_armstrong = (num) ->
  num == armstrong_sum num
  
{ :is_armstrong }
