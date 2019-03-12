local function is_prime(n)
    if n < 2      then return false end
    if n == 2     then return true  end
    if n % 2 == 0 then return false end
    for i = 3, math.floor(math.sqrt(n)), 2 do
        if n % i == 0 then return false end
    end
    return true
end

local prime_generator = function()
    return coroutine.wrap(
        function()
            coroutine.yield(2)
            local p = 3
            coroutine.yield(p)
            while true do
                repeat
                    p = p + 2
                until is_prime(p)
                coroutine.yield(p)
            end
        end
    ) 
end

local function nth_prime(n)
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
