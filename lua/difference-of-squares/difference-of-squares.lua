-- difference-of-squares

local function ident(x) return x end
local function square(x) return x^2 end

local function sum(n, inner, outer)
    local sum = 0
    for i = 1, n do
        sum = sum + inner(i)
    end
    return outer(sum)
end

local function square_of_sum(n)  return sum(n, ident, square) end
local function sum_of_squares(n) return sum(n, square, ident) end

return {
    square_of_sum = square_of_sum,
    sum_of_squares = sum_of_squares,
    difference_of_squares = function(n) return square_of_sum(n) - sum_of_squares(n) end,
}
