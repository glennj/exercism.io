'''
# recursive
def recite(start, take=1, lines=None):
    if take == 0:
        return lines

    # add the blank line between verses
    lines = [] if lines is None else lines + ['']
    lines += verse(start)

    return recite(start - 1, take - 1, lines)
'''

# iterative
def recite(start, take=1):
    lines = None

    for i in range(take):
        # add the blank line between verses
        lines = [] if lines is None else lines + ['']
        lines += verse(start)
        start -= 1

    return lines


def verse(n):
    b = bottle(n)
    first = f"{b.capitalize()} on the wall, {b}."
    b = bottle(99 if n == 0 else n - 1)
    second = f"{task(n)}, {b} on the wall."
    return (first, second)


def bottle(n):
    return "{} bottle{} of beer".format(
        n if n > 0 else "no more",
        "" if n == 1 else "s"
    )


def task(n):
    if n == 0:
        return "Go to the store and buy some more"
    else:
        return "Take {} down and pass it around".format(
            "one" if n > 1 else "it"
        )
