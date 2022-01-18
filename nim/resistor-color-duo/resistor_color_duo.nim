# this is from the resistor-color exercise
type
  ResistorColor* = enum
    Black, Brown, Red, Orange, Yellow,
    Green, Blue, Violet, Grey, White

proc colorCode(color: ResistorColor): int = color.ord

proc value*(colors: openArray[ResistorColor]): int =
  assert colors.len >= 2, "not enough colors"
  10 * colors[0].colorCode + colors[1].colorCode
