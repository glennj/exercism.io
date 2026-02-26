import fold from require 'moon'

-- does the table contain a target element
contains = (t, item) ->
  for elem in *t
    return true if elem == item
  false


--
map = (t, fn) ->
  result = {}
  for i, elem in pairs t
    result[i] = fn elem, i
  result


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


{ :contains, :map, :treverse, :clone, :sorted }
