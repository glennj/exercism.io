from string import ascii_lowercase as alphabet, digits
from math import gcd
import re

non_alnum = f'[^{alphabet}{digits}]'


def encode(plain_text, a, b):
    validate_key(a)

    def encoding(char):
        try:
            x = alphabet.index(char)
            char = alphabet[(a * x + b) % len(alphabet)]
        except ValueError:
            pass
        return char

    encoded = x_code(plain_text, encoding)
    return re.sub('.{5}', r'\g<0> ', encoded).rstrip(" ")


def decode(ciphered_text, a, b):
    validate_key(a)
    a_inv = mmi(a, len(alphabet))

    def decoding(char):
        try:
            y = alphabet.index(char)
            char = alphabet[(a_inv * (y - b)) % len(alphabet)]
        except ValueError:
            pass
        return char

    return x_code(ciphered_text, decoding)


def validate_key(a):
    if gcd(a, len(alphabet)) != 1:
        raise ValueError('a and m must be coprime.')


def mmi(a, m):
    for n in range(m):
        if (a * n) % m == 1:
            return n


def x_code(text, func):
    return ''.join(
        func(char)
        for char in re.sub(non_alnum, '', text.lower())
    )
