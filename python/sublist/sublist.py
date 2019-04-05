SUBLIST, SUPERLIST, EQUAL, UNEQUAL = range(4)


def check_lists(a, b):
    if len(a) == len(b):
        return EQUAL if is_sublist(a, b) else UNEQUAL
    if len(a) < len(b):
        return SUBLIST if is_sublist(a, b) else UNEQUAL
    if len(a) > len(b):
        return SUPERLIST if is_sublist(b, a) else UNEQUAL
        # .............................^..^


def is_sublist(a, b):
    if len(a) == 0:
        return True

    try:
        idx = b.index(a[0])
    except ValueError:
        idx = None

    while idx is not None:
        found = True
        for i in range(1, len(a)):
            if idx + i >= len(b):
                return False
            if a[i] != b[idx + i]:
                found = False
                break
        if found:
            return True

        try:
            idx = b.index(a[0], idx + 1)
        except ValueError:
            idx = None

    return False
