import re

def is_isogram(string):
    normalized = re.sub(r'\W', '', string.lower())

    '''
    for i in range(len(normalized)):
        if normalized.count(normalized[i], i+1) > 0:
            return False
    '''

    '''
    # This should be faster as it only examines
    # each character once.
    count = {}
    for c in normalized:
        count[c] = (count[c] if c in count else 0) + 1
        if count[c] > 1:
            return False

    return True
    '''

    return len(normalized) == len(set(normalized))
