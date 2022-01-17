def primes(limit):
    if limit < 2:
        return []

    # initialize the candidates array
    candidates = list(range(limit+1))
    candidates[0] = None
    candidates[1] = None

    # first prime
    remove_multiples_of(2, candidates, limit)

    # subsequent primes
    for i in range(3, limit, 2):
        if candidates[i] is not None:
            remove_multiples_of(i, candidates, limit)

    # return the prime numbers
    return [n for n in candidates if n is not None]


def remove_multiples_of(n, candidates, limit):
    step = n * (1 if n == 2 else 2)
    for i in range(n*n, limit+1, step):
        candidates[i] = None
