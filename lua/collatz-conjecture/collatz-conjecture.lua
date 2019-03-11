local collatz_conjecture

-- iterative
collatz_conjecture = function (n)
    if n <= 0 then error('Only positive numbers are allowed') end
    local steps = 0
    while n > 1 do
        if n % 2 == 0 then
            n = n / 2
        else
            n = 3 * n + 1
        end
        steps = steps + 1
    end
    return steps
end

-- recursive
collatz_conjecture = function (n, steps)
    steps = steps or 0
    if n <= 0 then 
        error('Only positive numbers are allowed')
    elseif n == 1 then
        return steps
    elseif n % 2 == 0 then
        return collatz_conjecture(n / 2, steps + 1)
    else
        return collatz_conjecture(3 * n + 1, steps + 1)
    end
end

return collatz_conjecture
