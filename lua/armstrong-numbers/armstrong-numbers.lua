local ArmstrongNumbers = {}

function ArmstrongNumbers.is_armstrong_number(number)
  local n = number
  local len = math.floor(math.log(n, 10)) + 1
  local sum = 0

  while n > 0 do
    local digit = n % 10
    n = n // 10
    sum = sum + digit ^ len
  end

  return sum == number
end

return ArmstrongNumbers
