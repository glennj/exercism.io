include std/sequence.e      -- remove_all
include std/math.e          -- sum
include std/regex.e as re

sequence doubled = {0, 2, 4, 6, 8, 1, 3, 5, 7, 9}

public function valid(sequence s) 
    sequence digits = remove_all(' ', s)    

    if length(digits) < 2 or re:has_match(re:new(`\D`), digits) then
        return 0
    end if

    -- convert characters to digits
    digits -= '0'

    -- starting with the next-to-last, double each 2nd digit
    for i = length(digits) - 1 to 1 by -2 do
        digits[i] = doubled[digits[i] + 1]
    end for

    return remainder(sum(digits), 10) = 0
end function
