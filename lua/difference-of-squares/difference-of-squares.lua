-- difference-of-squares

local function square_of_sum(n)
    local sum = 0
    for i = 1, n do
        sum = sum + i
    end
    return sum ^ 2
end

local function sum_of_squares(n)
    local sum = 0
    for i = 1, n do
        sum = sum + i ^ 2
    end
    return sum
end

local function difference_of_squares(n)
    return square_of_sum(n) - sum_of_squares(n)
end

return {
    square_of_sum = square_of_sum,
    sum_of_squares = sum_of_squares,
    difference_of_squares = difference_of_squares,
}
