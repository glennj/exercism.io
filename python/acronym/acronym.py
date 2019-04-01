import re


def abbreviate(words):
    # A letter preceded by 
    # a) the start of the string, or
    # b) a non-(letter or apostrophe)
    regex = r"(?:^|[^A-Z'])([A-Z])"

    return "".join(re.findall(regex, words.upper()))
