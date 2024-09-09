class Sieve
  @primes: (limit) ->
    flags = new Array(limit + 1).fill(true).fill(false, 0, 2)

    markMultiples = (p, step = if p == 2 then 2 else 2 * p) ->
      for i in [p * p .. limit] by step
        flags[i] = false

    markMultiples 2
    for i in [3 .. Math.floor Math.sqrt limit] by 2
      if flags[i]
        markMultiples i

    (i for _, i in flags when flags[i])

module.exports = Sieve
