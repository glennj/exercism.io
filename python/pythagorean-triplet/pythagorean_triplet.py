# full disclosure: stolen from Python example.py

from math import sqrt


def triplets_with_sum(perimeter):
    triplets = []
    for triplet in triplets_in_range(1, perimeter // 2):
        a, b, c = triplet
        if a+b+c == perimeter:
            triplets.append(triplet)
    return triplets


def triplets_in_range(start, end):
    triplets = []
    for limit in range(4, end+1, 4):
        for triplet in primitive_triplets(limit):
            x, y, z = triplet
            a, b, c = triplet
            while a < start:
                a += x
                b += y
                c += z
            while c <= end:
                triplets.append([a, b, c])
                a += x
                b += y
                c += z
    return triplets


def primitive_triplets(limit):
    triplets = []
    for m, n in euclidian_coprimes(limit):
        a = m**2 - n**2
        b = 2 * m * n
        c = m**2 + n**2
        if a > b:
            a, b = b, a
        triplets.append((a, b, c))
    return triplets


def euclidian_coprimes(limit):
    mn = limit // 2
    sq = int(sqrt(mn))
    pairs = []
    for n in range(1, sq+1):
        if mn % n == 0:
            m = mn // n
            if (m - n) % 2 == 1 and gcd(m, n) == 1:
                pairs.append((m, n))
    return pairs


def gcd(a, b):
    return a if b == 0 else gcd(b, a % b)
