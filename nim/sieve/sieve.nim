import sequtils


proc markMultiples(prime: int, candidates: var seq[bool]) =
  var step = if prime == 2: 2 else: prime * 2
  for i in countup(prime*prime, candidates.len-1, step):
    candidates[i] = false


proc primes*(limit: int): seq[int] =
  if limit >= 2:
    # initially, all numbers are candidates
    var candidates = repeat(true, limit+1)

    # mark all non-primes as not candidates
    markMultiples 2, candidates
    for p in countup(3, limit, 2):
      if candidates[p]:
        markMultiples p, candidates

    # the remaining candidates are prime
    # which is prettier?

    # for p in 2..limit:
    #   if candidates[p]:
    #     result.add p

    return (2..limit).toSeq.filterIt(candidates[it])
