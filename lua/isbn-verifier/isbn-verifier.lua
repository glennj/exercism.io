local seq = require('pl.seq')

local validate_isbn = function (input)
    isbn = (input or ""):gsub("-", "")

    -- 9 digits followed by a digit or X
    if not isbn:match("^" .. ("%d"):rep(9) .. "[%dX]$") then
        return false
    end

    -- convert chars to digits
    local digit_value = function(char)
        return (char == "X" and 10 or tonumber(char))
    end
    local digits = seq.map(digit_value, isbn:gmatch("."))

    -- get the ISBN sum
    local sum = 0
    local accumulator = function(idx, digit)
        sum = sum + digit * (11 - idx)
    end
    seq.foreach(seq.enum(digits), accumulator)

    return sum % 11 == 0
end

return { valid = validate_isbn }
