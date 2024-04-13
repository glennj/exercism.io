-- The bitwise functions in the Math lib only handle 32-bit integers

public function square(integer n)
    if n < 1 or n > 64 then
        return "square must be between 1 and 64"
    end if

    return power(2, (n - 1))
end function

public function totalgrains()
    return power(2, 64) - 1
end function
