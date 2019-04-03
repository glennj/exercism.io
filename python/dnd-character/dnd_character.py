from random import randrange


def modifier(constitution):
    return (constitution - 10) // 2


class Character:
    characteristics = [
        'strength', 'dexterity', 'constitution',
        'intelligence', 'wisdom', 'charisma'
    ]

    def __init__(self):
        for c in self.characteristics:
            setattr(self, c, self.ability())
        self.hitpoints = 10 + modifier(self.constitution)

    def ability(self):
        ''' the numeric ability for a characteristic '''
        return Dice(6, 4).roll().sum_of_top(3)

    def abilities(self):
        return {c: getattr(self, c) for c in self.characteristics}


class DiceException(Exception):
    pass


class Dice:
    def __init__(self, faces, num):
        self.faces = faces
        self.num = num

    def roll(self):
        self.rolls = [1 + randrange(self.faces) for _ in range(self.num)]
        return self

    def sum_of_top(self, n):
        return sum(self.top(n))

    def top(self, n):
        if not self.rolls:
            raise DiceException("You haven't rolled yet.")
        if n > len(self.rolls):
            raise DiceException("You haven't rolled that many.")

        return sorted(self.rolls)[-n:]
