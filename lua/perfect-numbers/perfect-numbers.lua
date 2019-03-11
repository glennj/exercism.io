local Set = require("Set")

local function divmod(numerator, denominator)
    return numerator // denominator, numerator % denominator
end

local function aliquot_sum(n)
    if n < 1 then error("invalid input") end

    local factors = Set:new()
    local sqrt = math.floor(math.sqrt(n))

    for factor = 1, sqrt do
        local div, rem = divmod(n, factor)
        if rem == 0 then
            factors:add(factor)
            factors:add(div)
        end
    end
    factors:remove(n)

    local sum = 0
    for _, num in pairs(factors:tolist()) do 
        sum = sum + num 
    end
    return sum
end

local function classify(n)
    local sum = aliquot_sum(n)
    return 
        sum == n and "perfect" or 
        (sum < n and "deficient" or "abundant")
end

return { aliquot_sum = aliquot_sum, classify = classify }

