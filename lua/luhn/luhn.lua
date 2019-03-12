local each_digit = function (digit_string)
    return coroutine.wrap(function()
        for i = 1, #digit_string do
            coroutine.yield(tonumber(digit_string:sub(i, i)))
        end
    end)
end

local sum = function (input)
    local sum = 0
    local double = (#input % 2) == 0
    for d in each_digit(input) do
        if double then
            d = 2 * d
            if d > 9 then 
                d = d - 9 
            end
        end
        sum = sum + d
        double = not double
    end
    return sum 
end

return {
    valid = function (input)
        if input:find("[^%d%s]") then
            return false
        end

        input = input:gsub("%D", "")
        if #input < 2 then
            return false
        end

        return sum(input) % 10 == 0
    end
}
