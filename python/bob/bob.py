def response(phrase):
    phrase = phrase.rstrip()

    shouting = phrase.isupper()
    asking   = phrase.endswith('?')
    silence  = phrase == ""

    if shouting and asking:
        return "Calm down, I know what I'm doing!"
    elif shouting:
        return "Whoa, chill out!"
    elif asking:
        return "Sure."
    elif silence:
        return "Fine. Be that way!"
    else:
        return "Whatever."
