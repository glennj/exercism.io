local
  fun ident x  = x
  fun square x = x * x

  fun getSum n f1 f2 = f2 (List.foldl op+ 0 (List.tabulate (n + 1, f1)))
in
  fun squareOfSum n  = getSum n ident square
  fun sumOfSquares n = getSum n square ident

  fun differenceOfSquares n = squareOfSum n - sumOfSquares n
end
