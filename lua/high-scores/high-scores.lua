-- A closure-based solution.
-- Colon notation will pass a "self" parameter to the functions:
-- we can ignore it.

return function(values)
  return {
    scores = function()
      return values
    end,

    latest = function()
      return values[#values]
    end,

    personal_best = function()
      return math.max(table.unpack(values))
    end,

    personal_top_three = function()
      local clone = { table.unpack(values) }
      table.sort(clone, function(a, b) return a > b end) -- reverse sort
      top3 = { table.unpack(clone, 1, 3) }
      return top3
    end
  }
end
