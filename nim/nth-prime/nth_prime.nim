import math, strformat

proc isqrt(n: int): int = n.toFloat.sqrt.floor.toInt

proc isPrime(n: int): bool =
  if n < 2:          false
  elif n == 2:       true
  elif n mod 2 == 0: false
  else:
    for i in countup(3, isqrt(n), 2):
      if n mod i == 0:
        return false
    true

proc primer(): iterator():int =
  return iterator(): int =
    yield 2
    var prime = 3
    while true:
      if prime.isPrime:
        yield prime
      prime.inc(2)

proc prime*(nth: int): int =
  if nth < 1:
    raise newException(ValueError, "there is no zeroth prime")
  let nextPrime = primer()
  for count in 1 .. nth:
    result = nextPrime()
