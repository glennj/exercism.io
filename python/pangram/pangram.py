from string import ascii_letters


def is_pangram(sentence):
    chars = [c for c in sentence.lower() if c in ascii_letters]
    return len(set(chars)) == 26
