
local classify_triangle = function (a, b, c)
    local sides = {a, b, c}
    table.sort(sides)
    a, b, c = table.unpack(sides)

    assert(a > 0 and a + b > c, 'Input Error')

    if a == b and a == c then
        return 'equilateral'
    elseif a == b or a == c or b == c then
        return 'isosceles'
    else
        return 'scalene'
    end
end

return { kind = classify_triangle }
