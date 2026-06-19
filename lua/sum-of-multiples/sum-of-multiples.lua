local sum_of_multiples = function(factors)

    local is_a_multiple = function (n)
        for _, f in pairs(factors) do
            if f ~= 0 and n % f == 0 then
                return true
            end
        end
        return false
    end

    local to = function (n)
        local sum = 0
        for i = 1, n-1 do
            if is_a_multiple(i) then
                sum = sum + i
            end
        end
        return sum
    end

    return { to = to }
end

return sum_of_multiples
