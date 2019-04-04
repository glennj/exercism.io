import re


vowel = re.compile(r'''
    ^(?:[aeiou]     # apple => appleay
        |xr         # xray => xrayay
        |yt         # yttria => yttriaay
     )
''', re.I | re.X)


consonant = re.compile(r'''
    ^(?:(.?qu)(.*)          # square => aresquay
        |([^aeiouy]+)(y.*)  # rhythm => ythmrhay
        |([^aeiou]+)(.*)    # clock => ockclay
     )
''', re.I | re.X)


def pig_latinize(match):
    word = match.group(0)

    m = re.match(vowel, word)
    if m:
        return f"{word}ay"

    m = re.match(consonant, word)
    if m:
        prefix, rest = [g for g in m.groups() if g is not None]
        pig = f"{rest}{prefix}ay"
        if word == word.capitalize():
            pig = pig.capitalize()
        return pig

    return word


def translate(text):
    return re.sub(r'\w+', pig_latinize, text)


if __name__ == '__main__':
    for word in ('Square', 'My', 'Cat'):
        print(word, translate(word))

    import sys
    sys.path += ['../say']
    from say import say
    print(translate(say(12345678)))
    # elvetway illionmay eethray undredhay anday ortyfay-ivefay ousandthay ixsay undredhay anday eventysay-eightay
