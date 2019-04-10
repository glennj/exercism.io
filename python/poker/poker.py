import re
from collections import defaultdict
from functools import reduce


def best_hands(hands):
    pokerhands = list(map(PokerHand, hands))
    groups = defaultdict(set)
    for hand in pokerhands:
        groups[hand.value].add(hand)

    best = groups[max(groups.keys())]
    if len(best) > 1:
        groups = defaultdict(set)
        for hand in best:
            groups[hand.ranking_value()].add(hand)
        best = groups[max(groups.keys())]

    # if there are multiple hands to return, what is the order?
    return sorted(map(str, best), reverse=True)


class PokerHand(object):
    def __init__(self, string):
        self.cards = list(map(Card, string.split()))
        uniq_cards = set(map(str, self.cards))
        if len(uniq_cards) < 5:
            raise ValueError('cheater!')
        self.groupings = self.find_groupings()
        self.ranking = []
        self.value = self.evaluate_hand()

    def __str__(self):
        return ' '.join(map(str, self.cards))

    def ranking_value(self):
        return reduce(
            lambda acc, n: acc * len(Card.FACES) + n,
            self.ranking,
            0
        )

    def find_groupings(self):
        '''
        Find the x-of-a-kind groups in this hand.
        Return a dict mapping the card value to the
        number of cards with that value.
        '''
        groups = defaultdict(int)
        for card in self.cards:
            groups[card.value()] += 1
        return groups

    def evaluate_hand(self):
        self.ranking = self.card_values_descending()
        return (
            9 if self.is_five_of_a_kind() else
            8 if self.is_straight_flush() else
            7 if self.is_four_of_a_kind() else
            6 if self.is_full_house() else
            5 if self.is_flush() else
            4 if self.is_straight() else
            3 if self.is_three_of_a_kind() else
            2 if self.is_two_pairs() else
            1 if self.is_one_pair() else
            0 
        )

    def is_five_of_a_kind(self):
        result = len(self.groupings) == 1
        if result:
            self.ranking = list(self.cards[0].value())
        return result

    def is_straight_flush(self):
        '''
        test in this order: if 2 straight flushes,
        winner goes to high card of straight.
        '''
        return self.is_flush() and self.is_straight()

    def is_four_of_a_kind(self):
        return self.has_groupings([4, 1])

    def is_full_house(self):
        return self.has_groupings([3, 2])

    def is_flush(self):
        suit = self.cards[0].suit()
        result = all(c.suit() == suit for c in self.cards)
        if result:
            self.ranking = self.card_values_descending()
        return result

    def is_straight(self):
        ''' Ace can be high or low for this hand. '''
        values = self.card_values_descending()
        if values == [12, 3, 2, 1, 0]:
            # A,5,4,3,2
            self.ranking = [3]
            return True
        first = values[0]
        straight = values == list(range(first, first-5, -1))
        if straight:
            self.ranking = [first]
        return straight

    def is_three_of_a_kind(self):
        return self.has_groupings([3, 1, 1])

    def is_two_pairs(self):
        return self.has_groupings([2, 2, 1])

    def is_one_pair(self):
        return self.has_groupings([2, 1, 1, 1])

    def has_groupings(self, group_counts):
        # recall, groupings maps card value to num of cards
        counts = sorted(self.groupings.values(), reverse=True)
        result = counts == group_counts
        if result:
            self.ranking = [
                value
                for value, count in sorted(
                    self.groupings.items(),
                    key=lambda item: (item[1], item[0]),
                    reverse=True
                )
            ]
        return result

    def card_values_descending(self):
        return sorted(
            map(Card.value, self.cards),
            reverse=True
        )


class Card(object):
    FACES = ['2', '3', '4', '5', '6', '7', '8', '9', '10',
             'J', 'Q', 'K', 'A']
    PATT = re.compile(r'^([2-9]|10|[JQKA])([CSDH])$')

    def __init__(self, string):
        m = self.PATT.match(string)
        if not m:
            raise ValueError('invalid card: ' + string)
        self._value = self.FACES.index(m.group(1))
        self._suit = m.group(2)

    def value(self):
        return self._value

    def suit(self):
        return self._suit

    def __str__(self):
        return self.FACES[self._value] + self._suit
