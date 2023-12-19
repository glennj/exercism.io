provide:
  char-is-alpha,
  char-is-non-word-char
end

fun char-is-alpha(character :: String) -> Boolean:
  doc: 'Is the character an ASCII letter?'
  (("A" <= character) and (character <= "Z")) or
  (("a" <= character) and (character <= "z"))
where:
  char-is-alpha("A") is true
  char-is-alpha("a") is true
  char-is-alpha(" ") is false
  char-is-alpha("-") is false
  char-is-alpha("_") is false
  char-is-alpha("'") is false
end

fun char-is-non-word-char(character :: String) -> Boolean:
  doc: 'Is the character NOT a "word character" (letters or apostrophes)?'
  not(char-is-alpha(character) or (character == "'"))
where:
  char-is-non-word-char("A") is false
  char-is-non-word-char("a") is false
  char-is-non-word-char(" ") is true
  char-is-non-word-char("-") is true
  char-is-non-word-char("_") is true
  char-is-non-word-char("'") is false
end
