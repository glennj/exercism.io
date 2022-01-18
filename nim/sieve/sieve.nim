import sequtils


proc markMultiples(prime: int, candidates: var seq[bool]) =
  var step = if prime == 2: 2 else: prime * 2
  for i in countup(prime*prime, candidates.len-1, step):
    candidates[i] = false


proc primes*(limit: int): seq[int] =
  if limit >= 2:
    var candidates = repeat(true, limit+1)
    markMultiples 2, candidates

    for p in countup(3, limit, 2):
      if candidates[p]:
        markMultiples p, candidates

    for p in 2..limit:
      if candidates[p]:
        result.add p
