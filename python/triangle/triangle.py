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


'''
community solution using validator:
https://exercism.io/tracks/python/exercises/triangle/solutions/ef7f2f708da0422381bfa8ed075096b9

    def valid_triangle(f):
        def inner(sides):
            s = sorted(sides)
            return s[0] + s[1] > s[2] and s[0] != 0 and f(sides)
        return inner


    @valid_triangle
    def is_equilateral(sides):
        return len(set(sides)) == 1


    @valid_triangle
    def is_isosceles(sides):
        return len(set(sides)) < 3


    @valid_triangle
    def is_scalene(sides):
        return len(set(sides)) == 3
'''
