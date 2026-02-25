import fold from require 'moon'

-- reverse the elements of a table in-place
treverse = (t) ->
  {i, j} = {1, #t}
  while i < j
    temp = t[i]
    t[i] = t[j]
    t[j] = temp
    i += 1
    j -= 1


-- return a shallow copy of a table
clone = (t) -> {table.unpack t}


-- the builtin `table.sort` sorts the table in-place and returns nil.
-- this function returns a sorted copy
sorted = (t) ->
  u = clone t
  table.sort u
  u


{ :treverse, :clone, :sorted }
