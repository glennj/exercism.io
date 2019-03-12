local function is_prime(n)
    if n < 2      then return false end
    if n == 2     then return true  end
    if n % 2 == 0 then return false end
    for i = 3, math.floor(math.sqrt(n)), 2 do
        if n % i == 0 then return false end
    end
    return true
end

local function next_prime()
    return coroutine.create(
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
    local i = 0
    local np = next_prime()
    local prime, status
    repeat
        status, prime = coroutine.resume(np)
        i = i + 1
    until i == n
    return prime
end

return nth_prime
