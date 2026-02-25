{
  primes: (limit) ->
    flags = with [true for _ = 1,limit]
      [1] = false

    mark_multiples = (n, step) ->
      flags[i] = false for i = n*n, limit, step

    mark_multiples 2, 2
    mark_multiples i, 2*i for i = 3, math.sqrt(limit), 2 when flags[i]

    [i for i, is_prime in ipairs flags when is_prime]
}
