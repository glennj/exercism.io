from functools import reduce


def product(nums):
    return reduce(lambda p, n: p * n, nums, 1)


def largest_product(series, size):
    if size > len(series):
        raise ValueError("span must not exceed string length")

    if series != "" and not series.isdecimal():
        raise ValueError('digits input must only contain digits')

    if size < 0:
        raise ValueError('span must not be negative')

    # max() throws ValueError on empty sequence
    # `series == ""` and `size == 0` edge cases are handled
    return max([
        product(map(int, series[i:i+size]))
        for i in range(len(series) - size + 1)
    ])
