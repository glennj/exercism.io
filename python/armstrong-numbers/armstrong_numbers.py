from functools import reduce
import math


def is_armstrong(number):
    if number < 1: raise ValueError('Positive numbers only')

    n = math.ceil(math.log10(number))
    sum = reduce(lambda s, d: s + d**n, map(int, str(number)), 0)

    '''
    # iterative
    tmp = number
    sum = 0
    while tmp > 0:
        tmp, digit = divmod(tmp, 10)
        sum += digit ** n
    '''

    return number == sum
