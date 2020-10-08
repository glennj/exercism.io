const MAP = [
  (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
  ( 100, "C"), ( 90, "XC"), ( 50, "L"), ( 40, "XL"),
  (  10, "X"), (  9, "IX"), (  5, "V"), (  4, "IV"),
  (   1, "I") ]

proc roman*(decimal: int): string =
  var n = decimal
  for (d, r) in MAP:
    while n >= d:
      result.add(r)
      n -= d
