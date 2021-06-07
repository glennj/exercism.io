def reverse(text):
    '''
    rev = ''
    for ch in text:
        rev = ch + rev
    return rev
    '''
    return "".join(c for c in reversed(text))
