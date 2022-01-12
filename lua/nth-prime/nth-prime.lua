-- a coroutine wrapped in a closure
local prime_generator = function()
    local primes = {}

    local is_prime = function(n)
        local result = true
        local sq = math.sqrt(n)
        for i = 1, #primes do
            local p = primes[i]
            if p > sq then break end
            if n % p == 0 then
                result = false
                break
            end
        end
        return result
    end

    return coroutine.wrap(
        function()
            coroutine.yield(2)          -- first prime
            table.insert(primes, 2)
            local p = 3                 -- second prime
            while true do
                coroutine.yield(p)
                table.insert(primes, p)
                repeat                  -- find the next one
                    p = p + 2
                until is_prime(p)
            end
        end
    ) 
end

local nth_prime = function(n)
    assert(n > 0)

    local next_prime = prime_generator()
    local i = 0
    local prime

    repeat
        prime = next_prime()
        i = i + 1
    until i == n
    return prime
end

return nth_prime
