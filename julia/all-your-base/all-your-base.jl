function all_your_base(digits, base_in, base_out)
    if base_in < 2 || base_out < 2
        throw(DomainError("base must be ≥ 2"))
    end

    # convert input digits to a decimal number
    value = 0
    for d in digits
        (0 ≤ d < base_in) || throw(DomainError("digit out of range"))
        value = value * base_in + d
    end

    # express decimal value as digits of output base
    result = []
    while true
        value, digit = divrem(value, base_out)
        pushfirst!(result, digit)
        if value == 0
            break
        end
    end
    result
end
