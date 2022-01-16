def is_paired(input_string):
    brackets = {']': '[', '}': '{', ')': '('}
    stack = []
    for ch in input_string:
        if ch in brackets.values():
            # an open bracket
            stack.append(ch)
        elif ch in brackets.keys():
            # a close bracket
            if len(stack) == 0:
                return False
            b = stack.pop()
            if b != brackets[ch]:
                return False
    return len(stack) == 0
