import math


def classify(number):
    if number < 1:
        raise ValueError('Positive numbers only.')

    factors = set([1])
    for i in range(2, 1+math.floor(math.sqrt(number))):
        q, r = divmod(number, i)
        if r == 0:
            factors.add(i)
            factors.add(q)

    summ = sum(factors)
    if summ < number or number == 1:
        return "deficient"
    if summ == number:
        return "perfect"
    return "abundant"
