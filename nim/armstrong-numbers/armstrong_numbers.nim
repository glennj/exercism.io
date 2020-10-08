# Without using string functions

import math

proc armstrongSum(anInteger: int): int =
  var n = anInteger
  var nDigits = n.toFloat.log10.ceil.toInt
  while n > 0:
    result.inc((n mod 10) ^ nDigits)
    n = n div 10

proc isArmstrongNumber*(n: int): bool =
  armstrongSum(n) == n
