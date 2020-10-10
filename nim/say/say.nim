# Inspired by this javascript solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

const Ones = [
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
  "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
]

const Tens = [ "", "",
  "twenty", "thirty", "forty", "fifty",
  "sixty", "seventy", "eighty", "ninety",
]

# need a forward declaration
proc say*(n: BiggestInt): string

proc saySmall(n: int): string =
  if n < 20:
    result = Ones[n]
  else:
    result = Tens[n div 10]
    var rem = n mod 10
    if rem > 0: result.add '-' & Ones[rem]

proc sayCompound(n: BiggestInt, suffix: string, divisor: int): string =
  result = (n div divisor).say & ' ' & suffix
  var rem = n mod divisor
  if rem > 0: result.add ' ' & rem.say


proc say*(n: BiggestInt): string =
  if n < 0:
    raise newException(ValueError, "out of range")
  elif n < 100:               saySmall(n.int)
  elif n < 1_000:             sayCompound(n, "hundred", 100)
  elif n < 1_000_000:         sayCompound(n, "thousand", 1_000)
  elif n < 1_000_000_000:     sayCompound(n, "million", 1_000_000)
  elif n < 1_000_000_000_000: sayCompound(n, "billion", 1_000_000_000)
  else:
    raise newException(ValueError, "out of range")
