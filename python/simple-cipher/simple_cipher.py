from itertools import cycle
from string import ascii_lowercase as alphabet
from secrets import choice


class Cipher(object):
    def __init__(self, key=None):
        if key is None:
            key = ''.join(choice(alphabet) for _ in range(100))
        elif not (key.isalpha() and key.islower()):
            raise ValueError('Invalid key')
        self.key = key

    def encode(self, text):
        return self._x_code(text.lower(), +1)

    def decode(self, text):
        return self._x_code(text.lower(), -1)

    def _x_code(self, text, dir):
        a = ord('a')
        key = cycle(self.key)
        encoded = ''
        for c in text:
            kc = next(key)
            if not c.isalpha():
                encoded += c
            else:
                i = ord(c) + dir * (ord(kc) - a)
                i = a + ((i - a) % len(alphabet))
                encoded += chr(i)
        return encoded
