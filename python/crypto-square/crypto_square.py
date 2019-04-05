import math


def encode(plain_text):
    text = ''.join(filter(str.isalnum, plain_text.lower()))
    size = math.ceil(math.sqrt(len(text)))
    if size == 0: return ''
    ciphers = [text[i::size] for i in range(size)]
    length = math.ceil(len(text) / size)
    return " ".join(
        (word + " "*length)[:length]
        for word in ciphers
    )
