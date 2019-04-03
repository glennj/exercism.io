from string import ascii_lowercase as low
from string import ascii_uppercase as up


def rotate(text, key):
    return text.translate(str.maketrans(
        low + up,
        low[key:] + low[:key] + up[key:] + up[:key]
    ))
