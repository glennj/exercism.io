'''docstring'''

def get_rounds(number):
    """

     :param number: int - current round number.
     :return: list - current round and the two that follow.
    """

    return list(range(number, number + 3))


def concatenate_rounds(rounds_1, rounds_2):
    """

    :param rounds_1: list - first rounds played.
    :param rounds_2: list - second set of rounds played.
    :return: list - all rounds played.
    """

    return rounds_1 + rounds_2


def list_contains_round(rounds, number):
    """

    :param rounds: list - rounds played.
    :param number: int - round number.
    :return:  bool - was the round played?
    """

    return number in rounds


def card_average(hand):
    """

    :param hand: list - cards in hand.
    :return:  float - average value of the cards in the hand.
    """

    return 1.0 * sum(hand) / len(hand)


def approx_average_is_average(hand):
    """

    :param hand: list - cards in hand.
    :return: bool - is approximate average the same as true average?
    """

    avg = card_average(hand)
    approx1 = (hand[0] + hand[-1]) / 2.0
    approx2 = hand[int(len(hand)/2)]

    return avg in (approx1, approx2)


def average_even_is_average_odd(hand):
    """

    :param hand: list - cards in hand.
    :return: bool - are even and odd averages equal?
    """

    evens = []
    odds = []
    for i, card in enumerate(hand):
        if i % 2 == 0:
            evens.append(card)
        else:
            odds.append(card)

    # alternate implementation: less efficient, but more concise
    #   evens = [card for i,card in enumerate(hand) if i % 2 == 0]
    #   odds  = [card for i,card in enumerate(hand) if i % 2 != 0]

    return card_average(evens) == card_average(odds)


def maybe_double_last(hand):
    """

    :param hand: list - cards in hand.
    :return: list - hand with Jacks (if present) value doubled.
    """

    if hand[-1] == 11:
        hand[-1] = 22
    return hand
