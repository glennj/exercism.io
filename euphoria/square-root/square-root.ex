-- Using the Binary numeral system (base 2) from Wikipedia
-- https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29

include std/math.e

public function squareRoot(integer n)
  -- find b, the greatest power of 4 <= n
  integer b = power(4, floor(log(n) / log(4)))
  integer x = 0

  while b != 0 do
    if n >= x + b then
      n = n - x - b
      x = shift_bits(x, 1) + b
    else
      x = shift_bits(x, 1)
    end if
    b = shift_bits(b, 2)
  end while

  return x
end function
