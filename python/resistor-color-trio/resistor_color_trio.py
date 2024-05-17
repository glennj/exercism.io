# pull in the code from the previous exercises
from resistor_color     import color_code
from resistor_color_duo import value


def three_band_value(colors):
    return value(colors[:2]) * pow(10, color_code(colors[2]))


def value_label(value):
    idx = 0
    while value > 0 and value % 1000 == 0:
        value //= 1000
        idx += 1
    prefix = ["", "kilo", "mega", "giga"][idx]
    return f'{value} {prefix}ohms'


def label(colors):
    return value_label(three_band_value(colors))
