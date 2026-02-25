import color_code from require './resistor_color'

value = (band1, band2) -> 10 * color_code(band1) + color_code(band2)

{ :value }
