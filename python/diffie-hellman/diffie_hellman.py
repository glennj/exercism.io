import random


def private_key(p):
    return random.randrange(2, p)


'''
ref: https://docs.python.org/3/library/functions.html#pow

pow(x, y[, z])
    Return x to the power y; if z is present, return x to the
    power y, modulo z (computed more efficiently than
    pow(x, y) % z).
'''


def public_key(p, g, private):
    # return (g ** private) % p
    return pow(g, private, p)


def secret(p, public, private):
    # return (public ** private) % p
    return pow(public, private, p)
