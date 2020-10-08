from strutils import isEmptyOrWhitespace


# the simple, cut'n'paste solution
proc convertSimple*(num: int): string =
  if num mod 3 == 0: result.add "Pling"
  if num mod 5 == 0: result.add "Plang"
  if num mod 7 == 0: result.add "Plong"
  if result.isEmptyOrWhitespace: result = $num


# a more (over?) engineered solution

const Drops = [(3, 'i'), (5, 'a'), (7, 'o')]

proc isDivisibleBy(numerator: int, denominator: int): bool =
  numerator mod denominator == 0

proc convert*(num: int): string =
  for (divisor, vowel) in Drops:
    if num.isDivisibleBy(divisor):
      result.add("Pl" & vowel & "ng")
  if result.isEmptyOrWhitespace:
    result = $num
