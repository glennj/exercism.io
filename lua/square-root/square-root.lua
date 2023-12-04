local SquareRoot = {}

-- Using the Binary numeral system (base 2) from Wikipedia
-- https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29

function SquareRoot.square_root(n)
  -- find b, the greatest power of 4 <= n
  local b = 4 ^ math.floor(math.log(n, 4))
  local x = 0

  while b ~= 0 do
    if n >= x + b then
      n = n - x - b
      x = (x >> 1) + b
    else
      x = x >> 1
    end
    b = b >> 2
  end

  return x
end

return SquareRoot
