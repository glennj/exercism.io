class CustomSet
  new: (items) =>
    @_set = {}
    @_size = 0
    @add item for item in *items

  add: (item) =>
    if not @_set[item]
      @_set[item] = true
      @_size += 1

  size: => @_size

  iter: =>
    -- ref http://lua-users.org/wiki/IteratorsTutorial
    iter_func = (t, item) ->
      item, _ = next t, item
      item
    return iter_func, @_set, nil

    -- or, the iterator can be implemented with a coroutine:
    --    coroutine.wrap ->
    --      for item, _ in pairs @_set
    --        coroutine.yield item

  is_empty: => not next @_set

  contains: (item) => @_set[item]

  is_subset: (other) =>
    return false if @size! > other\size!
    for item in @iter!
      return false if not other\contains item
    true

  is_disjoint: (other) =>
    for item in @iter!
      return false if other\contains item
    true

  -- the "eq" metamethod can be an instance method
  __eq: (other) =>
    @size! == other\size! and @is_subset other

  -- and an instance method can use a metamethod
  is_equal: (other) =>
    @ == other

  intersection: (other) =>
    inter = @@ {}
    for item in @iter!
      inter\add item if other\contains item
    inter

  difference: (other) =>
    diff = @@ {}
    for item in @iter!
      diff\add item if not other\contains item
    diff

  union: (other) =>
    union = @@ {}
    union\add item for item in @iter!
    union\add item for item in other\iter!
    union
