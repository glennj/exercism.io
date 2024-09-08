class Acronym
  @abbreviate: (phrase) ->
    state = 0
    acronym = ''
    for c in [phrase...]
      if state == 0  # seeking first letter 
        if c.isLetter()
          acronym += c
          state = 1
      else            # seeking end of word
        if not (c.isLetter() or c.isApostrophe())
          state = 0
    acronym.toUpperCase()

String::isLetter = -> /\p{Letter}/u.test(this)
String::isApostrophe = -> this.localeCompare("'") is 0

module.exports = Acronym
