import value from require './resistor_color_duo'
import color_code from require './resistor_color'

prefixes = {'', 'kilo', 'mega', 'giga'}

{
  label: (band1, band2, band3) ->
    resistance = value(band1, band2) * (10 ^ color_code band3)

    idx = 1
    while resistance > 0 and resistance % 1000 == 0
      resistance /= 1000
      idx += 1

    string.format '%d %sohms', resistance, prefixes[idx]
}
