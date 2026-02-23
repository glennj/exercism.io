{
  sum: (factors, limit) ->
    multiples = {}
    for f in *factors
      if f > 0
        m = f
        while m < limit
          multiples[m] = true
          m += f

    sum = 0
    sum += m for m in pairs multiples
    sum
}
