import strutils

proc abbreviate*(input: string): string =
  var state = 'A'

  for ch in input:
    if state == 'A':
      # state A: seeking the next letter to add to the acronym
      if ch.isAlphaAscii:
        result.add(ch)
        state = 'N'

    else:
      # state N: seeking the end of this sequence of
      # letters and apostrophes, so we can begin
      # seeking the next letter.
      if (ch.isAlphaAscii or ch == '\'').not:
        state = 'A'

  result.toUpperAscii
