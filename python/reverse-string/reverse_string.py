def reverse(text):
    '''
    rev = ''
    for ch in text:
        rev = ch + rev
    return rev
    '''
    return "".join(c for c in reversed(text))

    '''
    https://exercism.io/tracks/python/exercises/reverse-string/solutions/8071d07d3a5f423b93ddbb25bb4ccf21

    return str(text)[::-1]
    '''
