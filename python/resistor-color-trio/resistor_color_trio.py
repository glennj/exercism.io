# pull in the code from the previous exercises
from resistor_color     import color_code
from resistor_color_duo import value


def label(colors):
    val = value(colors[:2]) * pow(10, color_code(colors[2]))
    idx = 0
    while val > 0 and val % 1000 == 0:
        val //= 1000
        idx += 1
    prefix = ["", "kilo", "mega", "giga"][idx]
    return f'{val} {prefix}ohms'
