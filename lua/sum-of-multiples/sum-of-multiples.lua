local sum_of_multiples = function(multiples)

    local is_a_multiple = function (n)
        for _, m in pairs(multiples) do
            if n % m == 0 then
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
