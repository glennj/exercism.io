"""
state Alpha is searching for the next alpha to add to the acronym
state NonAlpha is searching for the next non-[alpha or hyphen]
"""
@enum State Alpha=1 NonAlpha=2

function acronym(phrase)
    acronym = ""
    state = Alpha

    for char in phrase
        if state == Alpha
            if isletter(char)
                acronym *= char
                state = NonAlpha
            end
        else
            if ! isletter(char) && char ≠ '\''
                state = Alpha
            end
        end
    end
    uppercase(acronym)
end
