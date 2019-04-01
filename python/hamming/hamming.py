def distance(strand_a, strand_b):
    if len(strand_a) != len(strand_b):
        raise ValueError('Strands must have same length.')

    '''
    dist = 0
    for i in range(len(strand_a)):
        if strand_a[i] != strand_b[i]:
            dist += 1
    return dist
    '''

    # return len([1 for a, b in zip(strand_a, strand_b) if a != b])

    return sum(1 for a, b in zip(strand_a, strand_b) if a != b)
