public function leap(integer year) 
  return divBy(year, 4) and not divBy(year, 100) or divBy(year, 400)
end function

function divBy(integer numerator, integer divisor)
  return remainder(numerator, divisor) = 0
end function
