{
  triplets_with_sum: (n) ->
    triplets = {}
    
    -- start with shortest side of smallest triplet {3, 4, 5}    
    for a = 3, n
      -- formula for b derived from:
      --    a + b + c == n
      --    a^2 + b^2 == c^2
      -- combined, we see
      --    a^2 + b^2 == (n - a - b)^2
      -- solve for b
      
      b = n * (n - 2 * a) / (2 * (n - a))
      break if b < a
      if b == math.tointeger b
        c = n - a - b
        table.insert triplets, {a, b, c}

    triplets
}
