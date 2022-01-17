def flatten(iterable):
    '''
    If the element has an `__iter__` attribute, then
    it supports `for ... in`. We should recursively flatten
    those elements.

    Except for strings: consider those as atomic.
    '''

    result = []
    for elem in iterable:
        result += \
            []     if elem is None else \
            [elem] if type(elem) == str else \
            [elem] if not hasattr(elem, '__iter__') else \
            flatten(elem)
    return result
