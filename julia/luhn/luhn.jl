function luhn(input::AbstractString)::Bool
    number = replace(input, r"\s" => "")
    occursin(r"\D", number) && return false

    digits = [parse(Int, d) for d in number]
    length(digits) < 2 && return false

    function luhn_digit(d, i)
        d *= iseven(i) ? 2 : 1
        d > 9 ? d - 9 : d
    end

    s = sum(
        luhn_digit(d, i) 
        for (i, d) in Iterators.enumerate(reverse(digits))
    )
    s % 10 == 0
end
