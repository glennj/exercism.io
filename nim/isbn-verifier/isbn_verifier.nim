import strutils

proc isValid*(input: string): bool =
  var sum = 0
  var i = 10
  var digit: int

  for c in input:
    case c
      of 'X':
        if i > 1: return false
        digit = 10
      of Digits: digit = parseInt($c)
      of '-': continue
      else: return false
    sum.inc digit * i
    i.dec

  if i != 0: return false
  sum mod 11 == 0
