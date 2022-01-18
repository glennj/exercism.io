import math

type
  ResistorColor* = enum
    Black, Brown, Red, Orange, Yellow,
    Green, Blue, Violet, Grey, White

proc colorCode(color: ResistorColor): int = color.ord

proc value(colors: openArray[ResistorColor]): int =
  assert colors.len >= 2, "not enough colors"
  10 * colors[0].colorCode + colors[1].colorCode

proc label*(colors: openArray[ResistorColor]): (int, string) =
  assert colors.len == 3, "incorrect input"
  var resistance = value([colors[0], colors[1]])
  resistance *= 10^colors[2].colorCode

  var magnitude = 0
  while resistance mod 1000 == 0:
    resistance = resistance div 1000
    magnitude.inc
  var prefix = ["", "kilo", "mega", "giga"][magnitude]

  (resistance, prefix&"ohms")
