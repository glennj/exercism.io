FACTORIALS = [1, 1]

def factorial(n):
    for i in range(len(FACTORIALS), n + 1):
        FACTORIALS.append( i * FACTORIALS[-1] )

    return FACTORIALS[n]


def choose(n, k):
    return factorial(n) / (factorial(k) * factorial(n - k))
