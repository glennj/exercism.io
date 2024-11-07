return { 
    to_decimal = function (value)
        local binary_string = tostring(value)
        local decimal = 0
        if binary_string:match("^[01]+$") then
            for char in binary_string:gmatch(".") do
                decimal = decimal * 2 + tonumber(char)
            end
        end
        return decimal
    end
}
