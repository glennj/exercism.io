def prime_factors(n):
    factors = []
    f = 2

    while f**2 <= n:
        if n % f == 0:
            factors.append(f)
            n //= f
        else:
            f += 1

    if n > 1:
        factors.append(n)

    return factors
