-- https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem
{
  maximumValue: (W, items) ->
    -- Create a matrix with the first column and row initialized to 0.
    -- Take care to create the zeroth index which is not naturally supported in Lua.
    m = [{[0]: 0} for i = 1, #items]
    m[0] = {w, 0 for w = 0, W}
    
    for i, item in ipairs items
      for w = 1, W
        m[i][w] = if item.weight > w
          m[i - 1][w]
        else
          math.max m[i - 1][w], m[i - 1][w - item.weight] + item.value
    m[#items][W]
}
