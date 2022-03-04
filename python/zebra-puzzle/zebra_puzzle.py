""" module docstring """

from itertools import permutations

############################################################
# The clues, and the method that implements them:
#   1. There are five houses.
#   2. The Englishman lives in the red house. (solve_for_nationality)
#   3. The Spaniard owns the dog. (solve_for_pets)
#   4. Coffee is drunk in the green house. (solve_for_beverages)
#   5. The Ukrainian drinks tea. (solve_for_beverages)
#   6. The green house is immediately to the right of the ivory house.
#      (solve_for_colour)
#   7. The Old Gold smoker owns snails. (solve_for_pets)
#   8. Kools are smoked in the yellow house. (solve_for_smokes)
#   9. Milk is drunk in the middle house. (solve_for_beverages)
#  10. The Norwegian lives in the first house. (solve_for_nationality)
#  11. The man who smokes Chesterfields lives in the house next to the man with
#      the fox. (solve_for_pets)
#  12. Kools are smoked in the house next to the house where the horse is kept.
#      (solve_for_pets)
#  13. The Lucky Strike smoker drinks orange juice. (solve_for_smokes)
#  14. The Japanese smokes Parliaments. (solve_for_smokes)
#  15. The Norwegian lives next to the blue house. (solve_for_nationality)
############################################################


HOUSES = [1, 2, 3, 4, 5]
FIRST = 1
MIDDLE = 3


def is_right_of(house1, house2):
    """ docstring """
    return house1 == house2 + 1


def is_next_to(house1, house2):
    """ docstring """
    return abs(house1 - house2) == 1


# yes, there are a lot of variables to keep track of...
# pylint: disable=too-many-instance-attributes
class ZebraPuzzle:
    """ docstring """

    def __init__(self):
        self.water_drinker = None
        self.zebra_owner = None
        self.nationalities = {}

        (self.red, self.green, self.ivory, self.yellow, self.blue) = [None] * 5
        (self.english, self.spanish, self.ukranian, self.norwegian, \
                self.japanese) = [None] * 5
        (self.coffee, self.tea, self.milk, self.orange_juice, \
                self.water) = [None] * 5
        (self.old_gold, self.kools, self.chesterfields, self.lucky_strike, \
                self.parliaments) = [None] * 5
        (self.dog, self.snails, self.fox, self.horse, self.zebra) = [None] * 5

    def solve(self):
        """ docstring """
        for perm in permutations(HOUSES):
            if self.solve_for_colour(perm):
                break

        return self

    def solve_for_colour(self, permutation):
        """ docstring """
        (self.red, self.green, self.ivory, self.yellow, self.blue) = permutation

        # clue 6
        if is_right_of(self.green, self.ivory):
            for perm in permutations(HOUSES):
                if self.solve_for_nationality(perm):
                    return True

        return False

    def solve_for_nationality(self, permutation):
        """ docstring """
        (self.english, self.spanish, self.ukranian, \
                self.norwegian, self.japanese) = permutation

        # clues 2, 10, 15
        if self.english == self.red \
                and self.norwegian == FIRST \
                and is_next_to(self.norwegian, self.blue):

            self.nationalities = {
                self.english: "Englishman",
                self.spanish: "Spaniard",
                self.ukranian: "Ukranian",
                self.norwegian: "Norwegian",
                self.japanese: "Japanese"
            }
            for perm in permutations(HOUSES):
                if self.solve_for_beverages(perm):
                    return True

        return False

    def solve_for_beverages(self, permutation):
        """ docstring """
        (self.coffee, self.tea, self.milk, \
                self.orange_juice, self.water) = permutation

        # clues 4, 5, 9
        if self.coffee == self.green \
                and self.ukranian == self.tea \
                and self.milk == MIDDLE:

            for perm in permutations(HOUSES):
                if self.solve_for_smokes(perm):
                    return True

        return False

    def solve_for_smokes(self, permutation):
        """ docstring """
        (self.old_gold, self.kools, self.chesterfields, \
                self.lucky_strike, self.parliaments) = permutation

        # clues 8, 13, 14
        if self.kools == self.yellow \
                and self.lucky_strike == self.orange_juice \
                and self.japanese == self.parliaments:

            for perm in permutations(HOUSES):
                if self.solve_for_pets(perm):
                    return True

        return False

    def solve_for_pets(self, permutation):
        """ docstring """
        (self.dog, self.snails, self.fox, self.horse, self.zebra) = permutation

        # clues 3, 7, 11, 12
        if self.spanish == self.dog \
                and self.old_gold == self.snails \
                and is_next_to(self.chesterfields, self.fox) \
                and is_next_to(self.kools, self.horse):

            self.water_drinker = self.nationalities[self.water]
            self.zebra_owner = self.nationalities[self.zebra]
            return True

        return False


############################################################

PUZZLE = ZebraPuzzle().solve()

def drinks_water():
    """ docstring """
    return PUZZLE.water_drinker

def owns_zebra():
    """ docstring """
    return PUZZLE.zebra_owner



# Very tidy generator-based solution:
#
#    https://exercism.org/tracks/python/exercises/zebra-puzzle/solutions/snzhr
#
# But, how is the `ppl` array ordered initially???
