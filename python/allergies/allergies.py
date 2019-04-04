class Allergies(object):
    allergens = [
        'eggs',
        'peanuts',
        'shellfish',
        'strawberries',
        'tomatoes',
        'chocolate',
        'pollen',
        'cats',
    ]

    def __init__(self, score):
        self.allergies = [
            allergen
            for i, allergen in enumerate(self.allergens)
            if (score & (1 << i)) != 0
        ]

    def is_allergic_to(self, item):
        return item in self.allergies

    @property
    def lst(self):
        return self.allergies
