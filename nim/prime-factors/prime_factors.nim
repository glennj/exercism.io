from math import sqrt, floor

proc isqrt(n: BiggestInt): int =
  n.toBiggestFloat.sqrt.floor.toInt

proc primeFactors*(n: BiggestInt): seq[int] =
  var n = n
  var f = 2
  while f <= n.isqrt:
    if n mod f == 0:
      result.add f
      n = n div f
    else:
      f.inc(if f == 2: 1 else: 2)
  if n > 1:
    result.add n.int
