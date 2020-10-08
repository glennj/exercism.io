import ../resistor-color/resistor_color
# add that import to the test suite as well.

proc value*(colors: openArray[ResistorColor]): int =
  assert colors.len >= 2, "not enough colors"
  10 * colors[0].colorCode + colors[1].colorCode
