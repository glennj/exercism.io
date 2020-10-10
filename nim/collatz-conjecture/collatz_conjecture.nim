proc isEven(n: int): bool = n mod 2 == 0

proc steps*(num: int, steps = 0): int =
  case num
    of 1: result = steps
    of 2..high(int):
      var next: int
      if num.isEven: next = num div 2
      else:          next = num * 3 + 1
      result = steps(next, steps + 1)
    else:
      raise newException(ValueError, "Positive ints only")
