from sequtils import anyIt


proc validate(digits: openArray[int], iBase: int, oBase: int): void =
  if iBase < 2 or oBase < 2:
    raise newException(ValueError, "Invalid base")
  if digits.anyIt(it < 0 or it >= iBase):
    raise newException(ValueError, "Invalid digit")


proc inputBaseToDecimal(digits: openArray[int], iBase: int): int =
  for digit in digits:
    result = iBase * result + digit


proc decimalToOutputBase(decimal: int, oBase: int): seq[int] =
  var dec = decimal
  if dec == 0:
    result.add 0
  while dec > 0:
    result.insert(dec mod oBase)
    dec = dec div oBase


proc convert*(digits: openArray[int], iBase: int, oBase: int): seq[int] =
  validate digits, iBase, oBase
  digits
    .inputBaseToDecimal(iBase)
    .decimalToOutputBase(oBase)

