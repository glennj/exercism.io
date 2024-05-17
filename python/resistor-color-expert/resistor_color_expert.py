# pull in the code from the previous exercises
from resistor_color import color_code


def tolerance(color):
    match color:
        case 'grey':
            return "0.05%"
        case 'violet':
            return "0.1%"
        case 'blue':
            return "0.25%"
        case 'green':
            return "0.5%"
        case 'brown':
            return "1%"
        case 'red':
            return "2%"
        case 'gold':
            return "5%"
        case 'silver':
            return "10%"
        case _:
            raise ValueError('Invalid tolerance color')


def with_tolerance(lbl, color):
    return f'{lbl} ±{tolerance(color)}'


def value(colors):
    val = 0
    for color in colors[0:-1]:
        val = 10 * val + color_code(color)
    return val * pow(10, color_code(colors[-1]))


# this differs from the resistor-color-trio label function
def label(val):
    idx = 0
    while val > 1000:
        val /= 1000
        idx += 1
    prefix = ["", "kilo", "mega", "giga"][idx]
    return f'{val:g} {prefix}ohms'


def resistor_label(colors):
    match len(colors):
        case 1:
            return label(value(['black', colors[0], 'black']))
        case 2:
            return label(value([*colors, 'black']))
        case 3:
            return label(value(colors))
        case 4 | 5:
            return with_tolerance(label(value(colors[:-1])), colors[-1])
        case _:
            pass
