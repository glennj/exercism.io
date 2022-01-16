# translation of this lovely recursive solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

SMALL = [
    'zero', 'one', 'two', 'three', 'four', 'five', 'six',
    'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve',
    'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen',
    'eighteen', 'nineteen'
]

TENS = {
    20: 'twenty', 30: 'thirty',  40: 'forty',  50: 'fifty',
    60: 'sixty',  70: 'seventy', 80: 'eighty', 90: 'ninety'
}


def say(n):
    n = int(n)
    if n < 0:    raise ValueError('input out of range')
    if n < 100:  return say_small(n)
    if n < 1e3:  return say_compound(n, 100, 'hundred')
    if n < 1e6:  return say_compound(n, 1e3, 'thousand')
    if n < 1e9:  return say_compound(n, 1e6, 'million')
    if n < 1e12: return say_compound(n, 1e9, 'billion')
    else:        raise ValueError('input out of range')


def say_small(n):
    return \
        SMALL[n] if n < len(SMALL) else \
        TENS[n]  if n in TENS else \
        "{}-{}".format(say(n - n % 10), say(n % 10))


def say_compound(n, base, word):
    n, rem = divmod(n, int(base))
    saying = [say(n), word]
    if rem > 0:
        saying += [say(rem)]
    return " ".join(saying)
