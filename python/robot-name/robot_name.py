import random
from string import ascii_uppercase, digits


class Robot(object):
    # shared by all robots
    names = set()

    def __init__(self):
        self.reset()

    def reset(self):
        while True:
            name = ''.join((
                random.choice(ascii_uppercase),
                random.choice(ascii_uppercase),
                random.choice(digits),
                random.choice(digits),
                random.choice(digits)
            ))
            if name not in self.names:
                break
        self.name = name
        self.names.add(name)
        return name
