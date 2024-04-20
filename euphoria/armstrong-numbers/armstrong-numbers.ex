include std/math.e

public function is_armstrong_number(integer number)
    return number = armstrong_sum(number)
end function

function armstrong_sum(integer number)
    integer sum = 0
    if number > 0 then
        integer len = floor(log10(number)) + 1
        integer n = number
        while n > 0 do
            integer digit = remainder(n, 10)
            sum += power(digit, len)
            n = floor(n / 10)
        end while
    end if
    return sum
end function
