import math


class ComplexNumber(object):
    def __init__(self, real, imaginary):
        self.r = real
        self.i = imaginary

    @property
    def real(self):
        return self.r

    @property
    def imaginary(self):
        return self.i

    def __eq__(self, other):
        return type(self) == type(other) and self.r == other.r and self.i == other.i

    def __add__(self, other):
        if type(self) == type(other):
            return ComplexNumber(self.r + other.r, self.i + other.i)
        else:
            return self + ComplexNumber(other, 0)

    def __radd__(self, other):
        return ComplexNumber(other, 0) + self

    def __sub__(self, other):
        if type(self) == type(other):
            return ComplexNumber(self.r - other.r, self.i - other.i)
        else:
            return self - ComplexNumber(other, 0)

    def __rsub__(self, other):
        return ComplexNumber(other, 0) - self

    def __mul__(self, other):
        if type(self) == type(other):
            r = self.r * other.r - self.i * other.i
            i = self.i * other.r + self.r * other.i
            return ComplexNumber(r, i)
        else:
            return self * ComplexNumber(other, 0)

    def __rmul__(self, other):
        return ComplexNumber(other, 0) * self

    def __truediv__(self, other):
        if type(self) == type(other):
            denom = other.r**2 + other.i**2
            r = (self.r * other.r + self.i * other.i) / denom
            i = (self.i * other.r - self.r * other.i) / denom
            return ComplexNumber(r, i)
        else:
            return self / ComplexNumber(other, 0)

    def __rtruediv__(self, other):
        return ComplexNumber(other, 0) / self

    def __abs__(self):
        return math.sqrt(self.r**2 + self.i**2)

    def conjugate(self):
        return ComplexNumber(self.r, -self.i)

    def exp(self):
        a = ComplexNumber(math.exp(self.r), 0)
        b = ComplexNumber(math.cos(self.i), math.sin(self.i))
        return a * b

    def __repr__(self):
        return f'{type(self).__name__}({self.r}, {self.i})'
