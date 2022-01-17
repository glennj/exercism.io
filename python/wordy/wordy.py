import re
import operator

operations = {
    'plus': operator.add,
    'minus': operator.sub,
    'multiplied by': operator.mul,
    'divided by': operator.floordiv
}

regex = {
    'number': re.compile(r'-?\d+'),
    'operator': re.compile(f'(?:{"|".join(operations.keys())})\\b')
}


def get_number(question):
    m = regex['number'].match(question)
    if not m:
        raise ValueError("syntax error")
    return [question.removeprefix(m.group(0)).lstrip(), int(m.group(0))]


def get_operation(question):
    m = regex['operator'].match(question)
    if not m:
        raise ValueError("unknown operation")
    return [question.removeprefix(m.group(0)).lstrip(), operations[m.group(0)]]


def prepare_question(question):
    q = question.lower().strip().removesuffix("?")

    # every valid question starts with "What is"
    prefix = "what is"
    if not q.startswith(prefix):
        raise ValueError("unknown operation")

    return q.removeprefix(prefix).lstrip()


def answer(question):
    q = prepare_question(question)

    # the question should start with a number
    q, result = get_number(q)

    while len(q) > 0:
        # can't have a number followed by a number
        if regex['number'].match(q):
            raise ValueError("syntax error")

        q, operation = get_operation(q)
        q, num = get_number(q)

        result = operation(result, num)

    return result
