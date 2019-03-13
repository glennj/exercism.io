local convert = function (digits, from_base)
    assert(math.tointeger(from_base) and from_base >= 2, 'invalid input base')

    local decimal = 0
    for _,digit in ipairs(digits) do
        assert(digit >= 0, 'negative digits are not allowed')
        assert(digit < from_base, 'digit out of range')
        decimal = decimal * from_base + digit
    end

    local do_conversion = function (to_base)
        assert(math.tointeger(to_base) and to_base >= 2, 'invalid output base')

        local to_digits = {}
        repeat
            local rem = decimal % to_base
            table.insert(to_digits, 1, rem)
            decimal = decimal // to_base
        until decimal == 0
        return to_digits
    end

    return { to = do_conversion }
end

return { convert = convert }
