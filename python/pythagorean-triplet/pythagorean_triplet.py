def triplets_with_sum(sum_of_triplet):
    triplets = set()
    '''
    The magic numbers 7, 4, 3 are derived from (3,4,5)
    being the smallest pythagorean triangle

    This is a brute force solution and will be slow for
    large sums.
    '''
    for c in range(sum_of_triplet - 7, 4, -1):
        for b in range(c - 1, 3, -1):
            a = sum_of_triplet - c - b
            if 3 <= a < b and a*a + b*b == c*c:
                triplets.add((a, b, c))
    return triplets

    '''
    # the set comprehension seems to be a lot slower
    return {
        (a, b, c)
        for c in range(sum_of_triplet - 7, 4, -1)
        for b in range(c - 1, 3, -1)
        for a in [sum_of_triplet - b - c]
        if 3 <= a < b and a*a + b*b == c*c
    }
    '''
