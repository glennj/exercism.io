import re
from itertools import permutations
from functools import reduce


def solve(puzzle):
    letters = sorted(set(re.findall('[A-Z]', puzzle)))
    words = re.findall('[A-Z]+', puzzle)

    for digits in permutations(range(10), len(letters)):
        values = dict(zip(letters, digits))
        operands = digitize(words, values)
        if operands:
            answer = operands.pop()
            if sum(operands) == answer:
                return values

        '''
        # this is more concise, but a lot slower
        values = dict(zip(letters, map(str, digits)))
        expr = re.sub('[A-Z]', lambda m: values[m.group(0)], puzzle)
        if (not re.search(r'\b0', expr)) and eval(expr):
            return {k: int(v) for k, v in values.items()}
        '''

    # no solution
    return {}


def digitize(words, values):
    numbers = []
    for word in words:
        if values[word[0]] == 0:
            # no number can start with 0
            return None
        numbers.append(
            reduce(lambda n, c: n*10 + values[c], word, 0)
        )
    return numbers


'''
* full test suite runs in about 62 sec on my slow VM
  - a very similar ruby implementationtook 163 sec
* the "slow" version ran in 467 sec
'''
