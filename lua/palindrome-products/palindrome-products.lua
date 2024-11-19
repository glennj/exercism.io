local reverseNumber = function(num)
    local rev = 0
    while num > 0 do
        rev = rev * 10 + num % 10
        num = num // 10
    end
    return rev
end

local isPalindrome = function(num)
    return num == reverseNumber(num)
end

local boundedFactors = function(num, min, max)
    local fs = {}
    local limit = math.min(max, math.floor(math.sqrt(num)))
    for f = min, limit do
        local g = num // f
        if f * g == num and g <= max then
            fs[#fs + 1] = {f, g}
        end
    end
    return fs
end

local solve = function(from, to, step, min, max)
    assert(min <= max, "min must be <= max")

    for prod = from, to, step do
        if isPalindrome(prod) then
            local fs = boundedFactors(prod, min, max)
            if #fs > 0 then
                return { value = prod, factors = fs }
            end
        end
    end

    return { value = nil, factors = {} }
end


return {
    smallest = function (min, max) return solve(min^2, max^2,  1, min, max) end,
    largest  = function (min, max) return solve(max^2, min^2, -1, min, max) end
}
