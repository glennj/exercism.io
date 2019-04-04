
def sum_of_multiples(limit, multiples):
    '''
    values = set()
    for m in multiples:
        if m > 0:
            values |= set(range(m, limit, m))
    return sum(values)
    '''

    '''
    from functools import reduce
    return sum(reduce(
        lambda values, m: values | set(range(m, limit, m)),
        [m for m in multiples if m > 0],
        set()
    ))
    '''

    # or, a *set* comprehension
    # ---------v
    return sum({
        n
        for m in multiples if m > 0
        for n in range(m, limit, m)
    })
