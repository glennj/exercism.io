# Using the Binary numeral system (base 2) from Wikipedia
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29

from math import *

def square_root(n):
    # find b, the greatest power of 4 <= n
    b = 4 ** int(log(n) / log(4))
    x = 0
    while b > 0:
        if n >= x + b:
            n = n - x - b
            x = b + (x >> 1)
        else:
            x = x >> 1
        b = b >> 2

    return x
