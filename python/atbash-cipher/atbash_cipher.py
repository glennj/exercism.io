from string import ascii_lowercase, punctuation, whitespace

TRANSLATION = str.maketrans(
    ascii_lowercase,
    ascii_lowercase[::-1],
    punctuation + whitespace
)


def x_code(string):
    return string.lower().translate(TRANSLATION)


def encode(string):
    return " ".join(chunks(x_code(string)))


decode = x_code


def chunks(string, size=5):
    # return re.findall(".{1,%d}" % (size), string)
    return [string[i:i+size] for i in range(0, len(string), size)]
