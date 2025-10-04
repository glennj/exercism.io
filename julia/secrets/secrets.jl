#=
The >> operator performs arithmetic shift, preserving the sign bit.
The >>> operator performs logical shift, padding with zeros as if the number was unsigned.
=#
shift_back(value, amount) = value >>> amount

set_bits(value, mask) = value | mask

flip_bits(value, mask) = xor(value, mask)

clear_bits(value, mask) = value & ~mask
