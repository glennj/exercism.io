import re
from collections import defaultdict


def word_count(phrase):
    count = defaultdict(int)

    # Specifically remove underscores. If python regular
    # expressions included POSIX character classes I wouldn't
    # have to do this, I'd use [[:alnum:]] in place of \w
    normalized = phrase.lower().replace('_', ' ')

    # Recognize a sequence of letters or numbers, where
    # single quotes can appear as "interior" characters.
    word_re = r"\b\w(?:[\w']*\w)?\b"

    for word in re.findall(word_re, normalized):
        count[word] += 1

    return count
