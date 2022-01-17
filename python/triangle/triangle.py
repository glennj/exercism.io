def equilateral(sides):
    return classify(sides) == 'equilateral'


def isosceles(sides):
    return classify(sides) in ('isosceles', 'equilateral')


def scalene(sides):
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
