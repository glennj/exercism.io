import math, sequtils, strutils


proc validate(digitString: string, size: int): void =
  if  size > digitString.len or
      (digitString.isEmptyOrWhitespace and size > 0) or
      size < 0 or
      digitString.anyIt(not it.isDigit):
    raise newException(ValueError, "some message here")


proc largestProduct*(digitString: string, size: int): int =
  validate digitString, size
  var digits = digitString.mapIt(parseInt($it))
  (0 .. digits.len-size)
    .mapIt( digits[it .. it+size-1].prod )
    .max
