import re
from functools import reduce


class Luhn(object):
    def __init__(self, card_num):
        self.no_space = re.sub(r'\s', '', str(card_num))
        self.digits = list(map(int, re.sub(r'\D', '', self.no_space)))

    def valid(self):
        valid = False
        if len(self.digits) >= 2 and len(self.no_space) == len(self.digits):
            summ = reduce(
                lambda acc, pair: acc + self.luhn_digit(*pair),
                enumerate(reversed(self.digits)),
                0)
            valid = summ % 10 == 0
        return valid

    def luhn_digit(self, idx, digit):
        d = digit * (2 if self.odd(idx) else 1)
        d -= 9 if d > 9 else 0
        return d

    def odd(self, n):
        return n % 2 == 1
