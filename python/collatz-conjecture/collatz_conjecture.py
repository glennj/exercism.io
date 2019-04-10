def collatz_steps(n):
    if n <= 0:
        raise ValueError('naughty')
    steps = 0
    while n != 1:
        n = 3 * n + 1 if n % 2 else n / 2
        steps += 1
    return steps
