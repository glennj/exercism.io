import math


class Rational(object):
    def __init__(self, numer, denom):
        self.numer, self.denom = reduced(numer, denom)

    def __eq__(self, other):
        return self.numer == other.numer and self.denom == other.denom

    def __repr__(self):
        return '{}/{}'.format(self.numer, self.denom)

    def __add__(self, other):
        n = self.numer * other.denom + self.denom * other.numer
        d = self.denom * other.denom
        return Rational(n, d)

    def __sub__(self, other):
        n = self.numer * other.denom - self.denom * other.numer
        d = self.denom * other.denom
        return Rational(n, d)

    def __mul__(self, other):
        n = self.numer * other.numer
        d = self.denom * other.denom
        return Rational(n, d)

    def __truediv__(self, other):
        n = self.numer * other.denom
        d = self.denom * other.numer
        return Rational(n, d)

    def __abs__(self):
        return Rational(abs(self.numer), abs(self.denom))

    def __pow__(self, power):
        if power >= 0:
            n = self.numer ** power
            d = self.denom ** power
        else:
            n = self.denom ** -power
            d = self.numer ** -power
        return Rational(n, d)

    def __rpow__(self, base):
        return nth_root(base ** self.numer, self.denom)


def nth_root(num, root):
    return math.e ** (math.log(num) / root)


def reduced(numer, denom):
    g = gcd(numer, denom)
    n = numer // g
    d = denom // g
    return (-n, -d) if d < 0 else (n, d)


def gcd(a, b):
    return b and gcd(b, a % b) or a
