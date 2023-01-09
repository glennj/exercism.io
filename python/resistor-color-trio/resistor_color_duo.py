# pull in the code from the previous exercise
from resistor_color import color_code
from functools import reduce


def value(colors):
    return reduce(
        lambda val, code: val * 10 + code,
        map(color_code, colors[:2])
    )
