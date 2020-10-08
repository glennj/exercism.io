import math
import ../resistor-color-duo/resistor_color_duo
import ../resistor-color/resistor_color
# add that import to the test suite as well.


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
