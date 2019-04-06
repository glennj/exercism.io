import math


def nth_prime(n):
    if n <= 0:
        raise ValueError('n is at least 1.')
    p = prime_generator()
    i = 0
    while i < n:
        prime = next(p)
        i += 1
    return prime


def prime_generator():
    def is_prime(n):
        nonlocal primes
        limit = math.floor(math.sqrt(n))
        for p in primes:
            if p > limit:
                break
            if n % p == 0:
                return False
        return True

    primes = [2]
    yield 2
    p = 3
    while True:
        yield p
        primes.append(p)
        while True:
            p += 2
            if is_prime(p):
                break
