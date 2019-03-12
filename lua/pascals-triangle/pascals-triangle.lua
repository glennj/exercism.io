local fact = {}

local function factorial(n)
    if n == 1 then return 1 end
    if fact[n] then return fact[n] end
    fact[n] = n * factorial(n-1)
    return fact[n]
end

local function binomial_coefficient(n, k)
    if k == 0 or (n-k) == 0 then return 1 end
    if k == 1 or (n-k) == 1 then return n end
    return factorial(n) / (factorial(k) * factorial(n-k))
end

local function generate_row(n)
    local row = {}
    for k = 1, n do
        row[#row+1] = binomial_coefficient(n-1, k-1)
    end
    return row
end

return function (n)
    local t = {}
    for i = 1, n do
        t[i] = generate_row(i)
    end

    return {
        rows = t,
        last_row = t[n],
    }
end
