def steps(n):
    if n <= 0:
        raise ValueError('Only positive numbers are allowed')
    steps = 0
    while n != 1:
        n = 3 * n + 1 if n % 2 else n / 2
        steps += 1
    return steps
