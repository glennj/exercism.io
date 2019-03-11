-- octal to decimal conversion
return function(value)
    local base = 8
    local octal_string = tostring(value)
    return {
        to_decimal = function()
            local decimal = 0
            for char in octal_string:gmatch(".") do
                local n = tonumber(char, base)
                if not n then return 0 end
                decimal = decimal * base + n
            end
            return decimal
        end
    }
end

--[[
    Octal = require("octal")

    -- can take a string
    dec = Octal("1234567").to_decimal()
    -- or a number
    dec = Octal(1234567).to_decimal()

    -- returns 0 for invalid octal numbers
    assert(0 == Octal(18).to_decimal())
--]]

