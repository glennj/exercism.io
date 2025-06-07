ResistorColor = require './resistor-color'
ResistorColorDuo = require './resistor-color-duo'

class ResistorColorTrio
  @label: (colors) ->
    val = ResistorColorDuo.value(colors)
    exp = ResistorColor.colorCode(colors[2])
    resistance = val * 10 ** exp

    idx = 0
    while resistance > 0 && resistance % 1000 == 0
      idx = idx + 1
      resistance = resistance / 1000

    "#{resistance} #{["", "kilo", "mega", "giga"][idx]}ohms"

module.exports = ResistorColorTrio
