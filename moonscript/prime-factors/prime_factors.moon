{
  factors: (n) ->
    fs = {}
    f = 2
    inc = 1

    while f * f <= n
      if n % f == 0
        table.insert fs, f
        n /= f
      else
        f += inc
        inc = 2

    if n > 1
      table.insert fs, n

    fs
}
