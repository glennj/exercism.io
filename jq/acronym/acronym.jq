.phrase
| [
    scan("[[:alpha:]][\\w']*")    # extract the words
    | explode                     # separate into codepoints
    | first
]
| implode                         # list of codepoints => string
| ascii_upcase