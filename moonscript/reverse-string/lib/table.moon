-- reverse the elements of a table in-place
treverse = (t) ->
  {i, j} = {1, #t}
  while i < j
    temp = t[i]
    t[i] = t[j]
    t[j] = temp
    i += 1
    j -= 1

{ :treverse }
