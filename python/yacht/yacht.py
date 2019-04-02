from functools import partial


def score_for(value, dice):
    return value * dice.count(value)

ONES, TWOS, THREES, FOURS, FIVES, SIXES = (
    partial(score_for, val) for val in range(1, 7)
)


def FULL_HOUSE(dice):
    uniq = set(dice)
    if len(uniq) == 2:
        if any(dice.count(d) == 3 for d in uniq):
            return sum(dice)
    return 0

def FOUR_OF_A_KIND(dice):
    fours = [d for d in set(dice) if dice.count(d) >= 4]
    return 4 * sum(fours)

def LITTLE_STRAIGHT(dice):
    return 30 if sorted(dice) == [1, 2, 3, 4, 5] else 0

def BIG_STRAIGHT(dice):
    return 30 if sorted(dice) == [2, 3, 4, 5, 6] else 0

def YACHT(dice):
    return 50 if len(set(dice)) == 1 else 0

CHOICE = sum


def score(dice, category):
    return category(dice)


""" take 1

from collections import defaultdict


# Score categories
ONES = 1
TWOS = 2
THREES = 3
FOURS = 4
FIVES = 5
SIXES = 6
FULL_HOUSE = 7
FOUR_OF_A_KIND = 8
LITTLE_STRAIGHT = 9
BIG_STRAIGHT = 10
CHOICE = 11
YACHT = 12


def score(dice, kind):
    dice = sorted(dice)

    if kind == LITTLE_STRAIGHT and dice == [1, 2, 3, 4, 5]:
        return 30

    if kind == BIG_STRAIGHT and dice == [2, 3, 4, 5, 6]:
        return 30

    if kind == CHOICE:
        return sum(dice)

    # The rest of the categories need to know the counts of
    # the dice rolled and/or the number of unique values.

    count = defaultdict(int)
    for value in dice:
        count[value] += 1
    values = sorted(list(count.keys()))

    if kind == YACHT and len(values) == 1:
        return 50

    if ONES <= kind <= SIXES:
        return kind * count[kind]

    if kind == FOUR_OF_A_KIND:
        if len(values) == 1:
            return 4 * values[0]
        elif len(values) == 2:
            for value in values:
                if count[value] == 4:
                    return 4 * value

    if kind == FULL_HOUSE:
        if len(values) == 2 and (
            (count[values[0]] == 2 and count[values[1]] == 3) or
            (count[values[0]] == 3 and count[values[1]] == 2)
        ):
            return sum(val * count[val] for val in values)

    return 0
"""


''' community
https://exercism.io/tracks/python/exercises/yacht/solutions/4d0f8194cb0147caaac9830aa97668b6
https://exercism.io/tracks/python/exercises/yacht/solutions/14b37855d11c4b2c91de4f30f21a2779
'''
