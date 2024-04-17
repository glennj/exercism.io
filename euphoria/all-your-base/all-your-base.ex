include lib/errors.e

public function rebase(integer input_base, sequence input_digits, integer output_base)
    sequence result = to_decimal(input_base, input_digits)
    if result[1] = ERR then
        return result
    end if
    return to_output_base(output_base, result[2])
end function

function to_decimal(integer base, sequence digits)
    if base < 2 then
        return {ERR, "input base must be >= 2"}
    end if

    atom decimal = 0
    for i = 1 to length(digits) do
        integer digit = digits[i]
        if digit < 0 or digit >= base then
            return {ERR, "all digits must satisfy 0 <= d < input base"}
        end if
        decimal = decimal * base + digit
    end for
    return {OK, decimal}
end function

function to_output_base(integer base, atom decimal)
    if base < 2 then
        return {ERR, "output base must be >= 2"}
    end if
    if decimal = 0 then
        return {OK, {0}}
    end if

    sequence digits = {}
    while decimal > 0 do
        digits = prepend(digits, remainder(decimal, base))
        decimal = floor(decimal / base)
    end while
    return {OK, digits}
end function
