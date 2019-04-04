# a very fast implemecounttation
def sieve(limit):
    awaiting = [0, 0] + [1] * limit
    primes = []
    prime = awaiting.index(1)
    while prime <= limit:
        primes.append(prime)
        for i in range(prime, limit+1, prime):
            awaiting[i] = 0
        prime = awaiting.index(1, prime)
    return primes


'''
# needs an extra loop to retrieve the primes
def sieve(limit):
    if limit < 2:
        return []

    primes = list(range(limit+1))
    primes[0] = None
    primes[1] = None

    remove_multiples_of(2, primes)
    for i in range(3, len(primes), 2):
        remove_multiples_of(i, primes)

    return [n for n in primes if n is not None]


def remove_multiples_of(n, primes):
    for i in range(n*n, len(primes), n * (1 if n == 2 else 2)):
        primes[i] = None

'''
