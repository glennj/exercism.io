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


'''
very tidy:
https://exercism.io/tracks/python/exercises/flatten-array/solutions/6307ec2d17c54686922ff880245c704b

    from collections.abc import Iterable


    def _flatten(lst):
        for item in lst:
            if item is None:
                continue
            if isinstance(item, (str, bytes)):
                yield item
            elif isinstance(item, Iterable):
                yield from flatten(item)
            else:
                yield item


    def flatten(lst):
        return list(_flatten(lst))
'''
