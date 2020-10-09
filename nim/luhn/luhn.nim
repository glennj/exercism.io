import algorithm, strutils

# pre-calculate the doubled digits:
const Digit = [
  # even-numbered indices are not doubled
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
  # odd-numbered indices are doubled
  [0, 2, 4, 6, 8, 1, 3, 5, 7, 9],
]

proc isValid*(input: string): bool =
  var sum = 0
  var idx = 0

  for c in input.reversed:
    case c
    of ' ':
      continue
    of Digits:
      sum.inc Digit[idx mod 2][parseInt($c)]
      idx.inc
    else:
      return false

  # single digit is invalid
  if idx == 1: return false

  sum mod 10 == 0
