from math import sqrt, floor

type
  PerfectNumber* = enum
    Perfect, Abundant, Deficient

# the largest int less than or equal to the square root
proc isqrt(n: int): int = n.toFloat.sqrt.floor.toInt

proc aliquotSum(n: int): int =
  var j: int
  for i in 1 .. isqrt n:
    if n mod i == 0:
      result.inc i
      j = n div i
      if i != j:
        result.inc j
  result - n


proc classify*(n: int): PerfectNumber =
  if n < 1:
    raise newException(ValueError, "cannot classify")

  let sum = aliquotSum n
  if sum > n:
    Abundant
  elif sum < n:
    Deficient
  else:
    Perfect
