-- A closure-based solution.
-- Colon notation will pass a "self" parameter to the functions:
-- we can ignore it.

return function(values)
  local sorted = { table.unpack(values) }             -- clone the input
  table.sort(sorted, function(a, b) return a > b end) -- reverse sort

  return {
    scores = function()
      return values
    end,

    latest = function()
      return values[#values]
    end,

    personal_best = function()
      return sorted[1]
    end,

    personal_top_three = function()
      return { table.unpack(sorted, 1, 3) }
    end
  }
end
