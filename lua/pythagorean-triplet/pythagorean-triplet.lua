local is_pythagorean = function (a, b, c)
    return (a^2 + b^2) == c^2
end

local triplets_with_sum = function (sum)
    local triplets = {}

    --[[ ** this nested loop is extremely slow **
    -- smallest pyth.triangle is {3,4,5} so we can use
    -- that in the loop endpoints to save a bit of time.
    for c = sum - 7, 5, -1 do
        for b = c-1, 4, -1 do
            local a = math.sqrt(c^2 - b^2)
            if  math.tointeger(a) 
                and a < b 
                and a+b+c == sum 
                and is_pythagorean(a, b, c)
            then
                table.insert(triplets, {math.tointeger(a), b, c})
            end
        end
    end
    --]]

    local a, b, c, numerator, denominator
    a = 0
    while true do
        a = a + 1
        numerator = sum * (sum - 2 * a)
        denominator = 2 * (sum - a)
        b = numerator // denominator
        if b < a then
            break
        end
        if numerator % denominator == 0 then
            c = sum - a - b
            table.insert(triplets, {a,b,c})
        end
    end

    return triplets
end

return triplets_with_sum
