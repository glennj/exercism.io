"""Calculate the number of grains on square `square`."""
function on_square(square)::UInt64
    1 <= square <= 64 || throw(DomainError("boo"))
    UInt64(2) ^ (square - 1)
end


"""Calculate the total number of grains after square `square`."""
function total_after(square)::UInt64
    1 <= square <= 64 || throw(DomainError("boo"))

    # UInt64(2) ^ square - 1
    # or
    # UInt64(1) << square - 1
    # or
    parse(UInt64, repeat('1', square), base=2)
end
