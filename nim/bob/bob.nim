import strutils


proc hey*(phrase: string): string =
  if phrase.isEmptyOrWhitespace:
    return "Fine. Be that way!"

  var shouting = phrase.contains({'A'..'Z'}) and not phrase.contains({'a'..'z'})
  var asking   = phrase.strip.endsWith('?')

  if shouting and asking:
    return "Calm down, I know what I'm doing!"
  elif shouting:
    return "Whoa, chill out!"
  elif asking:
    return "Sure."
  else:
    return "Whatever."
