return { 
    to_decimal = function (value)
        local binary_string = tostring(value)
        local decimal = 0
        if binary_string:match("^[01]+$") then
            for char in binary_string:gmatch(".") do
                decimal = decimal * 2
                if char == "1" then
                    decimal = decimal + 1
                end
            end
        end
        return decimal
    end
}

--[[
    binary = require("binary")

    -- can take a string
    assert(51 == binary.to_decimal("110011"))
    -- or a number
    assert(21 == binary.to_decimal(10101))

    -- returns 0 for invalid binary numbers
    assert(0 == binary.to_decimal(123))
--]]

