import math


def classify(number):
    if number < 1:
        raise ValueError('Positive numbers only.')

    factors = set()
    for i in range(1, 1+math.floor(math.sqrt(number))):
        q, r = divmod(number, i)
        if r == 0:
            factors.add(i)
            factors.add(q)
    factors.remove(number)

    summ = sum(factors)
    if summ < number:
        return "deficient"
    if summ == number:
        return "perfect"
    return "abundant"
