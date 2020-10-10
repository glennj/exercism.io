import strformat

proc recite*(words: openArray[string]): string =
  for i in 0 .. words.len - 2:
    result &= &"For want of a {words[i]} the {words[i+1]} was lost.\n"
  if words.len > 0:
    result &= &"And all for the want of a {words[0]}."
