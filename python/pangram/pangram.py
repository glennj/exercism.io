from string import ascii_letters


def is_pangram(sentence):
    chars = [c for c in sentence.lower() if c in ascii_letters]
    return len(set(chars)) == 26


''' community

https://exercism.io/tracks/python/exercises/pangram/solutions/49fb9703f8d6437ba2edf30f93ef61f7

    return all(letter in sentence.lower() for letter in ascii_lowercase)

https://exercism.io/tracks/python/exercises/pangram/solutions/66c0c33fd2774c6d9d032b16fae5034d

    from string import ascii_lowercase

    ALPHABET = set(ascii_lowercase)

    def is_pangram(string):
        return ALPHABET.issubset(string.lower())

https://exercism.io/tracks/python/exercises/pangram/solutions/77a6b978c097486cbe4ac31afa65b075

    sentence_set = set(sentence.lower())
    alpha_set = set(ascii_lowercase)
    return alpha_set - sentence_set == set([]):

'''
