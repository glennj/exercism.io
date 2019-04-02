import re


def verify(isbn):
    isbn = (isbn or "").replace('-', '')
    if not re.match(r'^\d{9}[\dX]$', isbn):
        return False

    digits = [d == 'X' and 10 or int(d) for d in isbn]
    summ = sum(d * (10 - i) for i, d in enumerate(digits))
    return summ % 11 == 0
