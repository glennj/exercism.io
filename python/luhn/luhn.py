import re
from functools import reduce


class Luhn(object):
    def __init__(self, card_num):
        no_space = re.sub(r'\s', '', str(card_num))
        digits = list(map(int, re.sub(r'\D', '', no_space)))

        self._valid = False
        if len(digits) >= 2 and len(no_space) == len(digits):
            summ = reduce(
                lambda acc, pair: acc + luhn_digit(*pair),
                enumerate(reversed(digits)),
                0)
            self._valid = summ % 10 == 0

    def is_valid(self):
        return self._valid


def luhn_digit(idx, digit):
    d = digit * (2 if odd(idx) else 1)
    d -= 9 if d > 9 else 0
    return d


def odd(n):
    return n % 2 == 1
