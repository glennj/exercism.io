include std/math.e

-- Using bitwise functions reveals the reality of Euphoria's 32-bit integers

public function square(integer n)
    if n < 1 or n > 64 then
        return "square must be between 1 and 64"
    end if

    -- return shift_bits(1, -(n - 1))
    return power(2, (n - 1))
end function

public function totalgrains()
    -- return shift_bits(1, -64) - 1
    return power(2, 64) - 1
end function
