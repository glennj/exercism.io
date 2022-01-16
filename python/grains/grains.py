def square(integer_number):
    if not 1 <= integer_number <= 64:
        raise ValueError('square must be between 1 and 64')
    return 1 << (integer_number - 1)


def total():
    return (1 << 64) - 1
