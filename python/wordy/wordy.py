import re
import operator

operations = {
    'plus': operator.add,
    'minus': operator.sub,
    'multiplied by': operator.mul,
    'divided by': operator.floordiv
}

ops = r'\b' + '|'.join(operations.keys()) + r'\b'
eqn = r'(?<=\s)((-?\d+)\s+(' + ops + r')\s+(-?\d+))'

ops_patt = re.compile(ops)
eqn_patt = re.compile(eqn)


def calculate(question):
    if not re.search(r'\d+\?$', question):
        raise ValueError('Ill-formatted question')

    while True:
        if not re.search(ops_patt, question):
            break

        m = re.search(eqn_patt, question)
        if not m:
            raise ValueError('Missing operands')

        eqn, a, op, b = [m.group(i) for i in range(1, 5)]
        val = operations[op](int(a), int(b))
        question = question.replace(eqn, str(val))

    for answer, next_chars in re.findall(r'(-?\d+)(\?$|\s+[a-z]*)', question):
        if next_chars == '?':
            return int(answer)
        else:
            raise ValueError('Unimplemented operation: ' + next_chars)
    raise ValueError('no digits')
