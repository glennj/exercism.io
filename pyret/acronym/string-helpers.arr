provide:
  string-is-alpha,
  string-is-non-word-char
end

fun string-is-alpha(character :: String) -> Boolean:
  doc: 'Is the character an ASCII letter?'
  (("A" <= character) and (character <= "Z")) or
  (("a" <= character) and (character <= "z"))
where:
  string-is-alpha("A") is true
  string-is-alpha("a") is true
  string-is-alpha(" ") is false
  string-is-alpha("-") is false
  string-is-alpha("_") is false
  string-is-alpha("'") is false
end

fun string-is-non-word-char(character :: String) -> Boolean:
  doc: 'Is the character NOT a "word character" (letters or apostrophes)?'
  not(string-is-alpha(character) or (character == "'"))
where:
  string-is-non-word-char("A") is false
  string-is-non-word-char("a") is false
  string-is-non-word-char(" ") is true
  string-is-non-word-char("-") is true
  string-is-non-word-char("_") is true
  string-is-non-word-char("'") is false
end
