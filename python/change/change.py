import sys


def find_fewest_coins(coins, amount):
    if amount < 0:
        raise ValueError("target can't be negative")
    _, S = change(amount, coins)
    return make_change(amount, coins, S)


def change(amount, coins):
    '''
    Change making algorithm from
    http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

    This function generates two arrays:

    C = maps the minimum number of coins required to make change
        for each n from 1 to amount.  It is returned but only
        used internally in this application.

    S = the _first_ coin used to make change for amount n
        (actually stores the coin _index_ into the coins array)
    '''
    C = [0] + [sys.maxsize] * amount
    S = [0] + [None] * amount

    for p in range(1, 1 + amount):
        min = sys.maxsize
        coin = None
        for i in range(len(coins)):
            if coins[i] <= p:
                if 1 + C[p - coins[i]] < min:
                    min = 1 + C[p - coins[i]]
                    coin = i
        C[p] = min
        S[p] = coin
    return C, S


def make_change(amount, coins, S):
    change = []
    if S[amount] is None:
        raise ValueError("can't make target with given coins")
    while amount > 0:
        coin = coins[S[amount]]
        change.append(coin)
        amount -= coin
    return sorted(change)
