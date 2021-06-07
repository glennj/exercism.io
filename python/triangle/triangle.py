def is_equilateral(sides):
    return classify(sides) == 'equilateral'


def is_isosceles(sides):
    return classify(sides) in ('isosceles', 'equilateral')


def is_scalene(sides):
    return classify(sides) == 'scalene'


def classify(sides):
    a, b, c = sorted(sides)
    if a <= 0 or (a + b) <= c:
        # not a triangle
        return

    if a == b and a == c:
        return 'equilateral'
    elif a == b or a == c or b == c:
        return 'isosceles'
    else:
        return 'scalene'
