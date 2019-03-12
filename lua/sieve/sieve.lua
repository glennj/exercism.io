local remove_multiples

local sieve = function (n)
    return coroutine.create(function()
        -- no primes less than 2
        if n < 2 then
            coroutine.yield()
            return
        end

        local primes = {}
        for i = 1, n do primes[#primes+1] = i end

        remove_multiples(2, primes)
        coroutine.yield(2)

        for i = 3, n, 2 do
            -- this odd number may not be prime
            if primes[i] then
                remove_multiples(i, primes)
                coroutine.yield(i)
            end
        end
    end)
end

remove_multiples = function (m, list)
    local step = (m == 2 and 1 or 2) * m
    for i = m*m, #list, step do
        list[i] = false
    end
end

return sieve
