import sequtils

proc sum*(limit: int, factors: seq[int]): int =

  proc isMultiple(n: int): bool =
    anyIt(factors, it > 0 and n mod it == 0)

  var multiples = filterIt(toSeq(0 ..< limit), isMultiple(it))

  if multiples.len == 0:
    0
  else:
    foldl(multiples, a + b)
