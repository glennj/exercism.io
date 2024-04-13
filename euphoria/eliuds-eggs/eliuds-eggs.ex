include std/math.e

public function eggCount(integer number) 
    integer count = 0
    while number > 0 do
        count += and_bits(number, 1)
        number = shift_bits(number, 1)
    end while
    return count
end function
