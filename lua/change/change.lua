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

local change = function(amount, denominations)
    local C = {[0] = 0}
    local S = {}

    for p = 1, amount do
        local min = math.maxinteger
        local coin

        for i = 1, #denominations do
            if denominations[i] <= p then
                if C[p - denominations[i]] < min then
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

local make_change = function(S, d, n)
    local result = {}
    while n > 0 do
        local coin = d[S[n]]
        assert(coin, "can't make target with given coins")

        table.insert(result, 1, coin)
        n = n - coin
    end
    return result
end
  
return function(amount, coins)
    assert(amount >= 0, "target can't be negative")
    local _,first_coin = change(amount, coins)
    return make_change(first_coin, coins, amount)
end
