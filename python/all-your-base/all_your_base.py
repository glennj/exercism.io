def rebase(input_base, digits, output_base):
    if input_base < 2 or output_base < 2:
        raise ValueError('invalid base')
    if not all(0 <= d < input_base for d in digits):
        raise ValueError('invalid digit')

    decimal = 0
    for d in digits:
        decimal = decimal * input_base + d

    out_digits = []
    while decimal > 0:
        decimal, digit = divmod(decimal, output_base)
        out_digits.insert(0, digit)

    return out_digits
