-- Change making algorithm from
-- http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

-- This function generates two arrays:
--
-- C = maps the minimum number of coins required to make
--     change for each n from 1 to amount.
--     It is returned but only used internally in this
--     application.
--
-- S = the _first_ coin used to make change for amount n
--     (actually stores the coin _index_ into the
--     denominations array)
--
local change = function(amount, denominations)
  local C = {[0] = 0}
  local S = {}

  for p = 1, amount do
    local min = math.maxinteger
    local coin

    for i = 1, #denominations do
      if denominations[i] <= p then
        if 1 + C[p - denominations[i]] < min then
          min = 1 + C[p - denominations[i]]
          coin = i
        end
      end
    end

    C[p] = min
    S[p] = coin
  end

  return C, S
end

-- determine hwo many of each coin to use
local make_change = function(S, d, n)
  local result = {}
  for i = 1, #d do result[i] = 0 end

  if not S[n] then 
    -- we can't render this amount with these coins
    return 
  end

  while n > 0 do
    local i = S[n]
    result[i] = result[i] + 1
    n = n - d[S[n]]
  end
  return result
end
  
return function(amount, coins)
  local _,first_coin = change(amount, coins)
  return make_change(first_coin, coins, amount)
end
