from math import floor, ceil
import sys


def largest(max_factor, min_factor):
    return palindrome_products(max_factor, min_factor)[1]


def smallest(max_factor, min_factor):
    return palindrome_products(max_factor, min_factor)[0]


def palindrome_products(maxf, minf):
    if minf > maxf:
        raise ValueError('min must be <= max')

    '''
    don't need to store **all** the palindrome products,
    just the min and max
    '''
    max_prod = -1
    max_result = (None, [])
    min_prod = sys.maxsize
    min_result = (None, [])

    for i in range(minf, maxf+1):
        for j in range(i, maxf+1):
            prod = i * j
            if is_palindrome(prod):
                if prod < min_prod:
                    min_prod = prod
                    min_result = (prod, set())
                if prod == min_prod:
                    min_result[1].add((i,j))

                if prod > max_prod:
                    max_prod = prod
                    max_result = (prod, set())
                if prod == max_prod:
                    max_result[1].add((i,j))

    return (min_result, max_result)


def is_palindrome(number):
    s = str(number)
    return s[:floor(len(s)/2)] == s[-1:ceil(len(s)/2)-1:-1]
