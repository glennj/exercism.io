local validate_isbn = function (input)
    isbn = (input or ""):gsub("-", "")

    -- 9 digits followed by a digit or X
    if not isbn:match("^" .. ("%d"):rep(9) .. "[%dX]$") then
        return false
    end

    -- convert chars to digits
    local digits = {}
    for i = 1, #isbn-1 do
        digits[#digits+1] = tonumber(isbn:sub(i,i))
    end
    local check = isbn:sub(-1)
    digits[#digits+1] = check == "X" and 10 or tonumber(check)

    -- apply ISBN formula
    local sum = 0
    for i, d in ipairs(digits) do
        sum = sum + d * (11 - i)
    end
    return sum % 11 == 0
end

return { valid = validate_isbn }
