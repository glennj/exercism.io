-- size of a table. `#` only applies to sequences
size = (t) ->
  n = 0
  n += 1 for _ in pairs t
  n


-- are two tables equal
equals = (t1, t2) ->
  return false if size(t1) != size(t2)
  for k, v in pairs t1
    return false if v != t2[k]
  true


-- does the table (sequence) contain a target element
contains = (t, item) ->
  for elem in *t
    if type(item) == "table"
      return true if type(elem) == "table" and equals item, elem
    elseif elem == item
      return true 
  false


-- distinct elements in sequence
distinct = (t) ->
  seen = {}
  result = {}
  for elem in *t
    if not seen[elem]
      seen[elem] = true
      table.insert result, elem
  result


--
map = (t, fn) ->
  result = {}
  for i, elem in pairs t
    result[i] = fn elem, i
  result


-- reverse the elements of a table in-place
treverse = (t) ->
  i, j = 1, #t
  while i < j
    t[i], t[j] = t[j], t[i]
    i += 1
    j -= 1


-- return a shallow copy of a table (sequence)
clone = (t) -> {table.unpack t}


-- the builtin `table.sort` sorts the table in-place and returns nil.
-- this function returns a sorted copy
sorted = (t) ->
  u = clone t
  table.sort u
  u


intersection = (t1, t2) ->
  result = {}
  for elem in *t1
    if contains t2, elem
      table.insert result, elem
  result


{
  :clone,
  :contains,
  :distinct,
  :equals,
  :intersection,
  :map,
  :size,
  :sorted,
  :treverse,
}
