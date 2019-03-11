local function add(roman, numeral, decimal, amount)
    table.insert(roman, numeral)
    return decimal - amount
end

local function to_roman(n)
    local roman = {}
    while n >= 1000 do   n = add(roman,  "M", n, 1000) end
    if    n >=  900 then n = add(roman, "CM", n,  900) end
    if    n >=  500 then n = add(roman,  "D", n,  500) end
    if    n >=  400 then n = add(roman, "CD", n,  400) end
    while n >=  100 do   n = add(roman,  "C", n,  100) end
    if    n >=   90 then n = add(roman, "XC", n,   90) end
    if    n >=   50 then n = add(roman,  "L", n,   50) end
    if    n >=   40 then n = add(roman, "XL", n,   40) end
    while n >=   10 do   n = add(roman,  "X", n,   10) end
    if    n >=    9 then n = add(roman, "IX", n,    9) end
    if    n >=    5 then n = add(roman,  "V", n,    5) end
    if    n >=    4 then n = add(roman, "IV", n,    4) end
    while n >=    1 do   n = add(roman,  "I", n,    1) end
    return table.concat(roman)
end

return { to_roman = to_roman }
