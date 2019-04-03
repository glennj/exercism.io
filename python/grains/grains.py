def on_square(integer_number):
    if not 1 <= integer_number <= 64:
        raise ValueError('Invalid square.')
    return 1 << (integer_number - 1)


def total_after(integer_number):
    return (on_square(integer_number) << 1) - 1
