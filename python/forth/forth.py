import operator


class StackUnderflowError(Exception):
    pass


class Stack(list):
    ''' Redefining list.pop '''
    def pop(self, num=1):
        if len(self) < num:
            raise StackUnderflowError('Insufficient number of items in stack')
        values = self[-num:]
        del self[-num:]
        return values if num > 1 else values[0]

    def push(self, *items):
        for item in items:
            self.append(item)

    def peek(self):
        val = self.pop()
        self.push(val)
        return val


def evaluate(input_data):
    stack = Stack()
    macros = dict()

    for cmd in input_data:
        words = cmd.upper().split()
        while len(words) > 0:
            word = words.pop(0)
            if is_number(word):
                stack.push(int(word))
            elif word in macros:
                words = macros[word] + words
            elif word == ':':
                record_macro(words, macros)
                del words[:]
            elif word == '+':
                binary_op(stack, operator.add)
            elif word == '-':
                binary_op(stack, operator.sub)
            elif word == '*':
                binary_op(stack, operator.mul)
            elif word == '/':
                check_div_by_zero(stack)
                binary_op(stack, operator.floordiv)
            elif word == 'DUP':
                a = stack.pop()
                stack.push(a, a)
            elif word == 'DROP':
                stack.pop()
            elif word == 'SWAP':
                a, b = stack.pop(num=2)
                stack.push(b, a)
            elif word == 'OVER':
                a, b = stack.pop(num=2)
                stack.push(a, b, a)
            else:
                raise ValueError('undefined operation')
    return stack


def is_number(word):
    try:
        num = int(word)
        return True
    except ValueError:
        return False


def check_div_by_zero(stack):
    if len(stack) > 0 and stack.peek() == 0:
        raise ZeroDivisionError('divide by zero')


def binary_op(stack, func):
    a, b = stack.pop(num=2)
    stack.push(func(a, b))


def record_macro(words, macros):
    name = words.pop(0)
    if is_number(name):
        raise ValueError('illegal operation')
    if words[-1] != ';':
        raise ValueError('macro not terminated with ;')
    words.pop()

    # macros definitions can contain macros. Expand them.
    while True:
        has_expanded, words = expand_macros_in_defn(words, macros)
        if not has_expanded:
            break
    macros[name] = words[:]  # copy


def expand_macros_in_defn(words, macros):
    has_expanded = False
    new_words = list()
    for word in words:
        if word in macros:
            new_words += macros[word]
            has_expanded = True
        else:
            new_words.append(word)
    return has_expanded, new_words
