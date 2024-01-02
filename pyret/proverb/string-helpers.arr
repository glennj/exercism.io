provide:
  string-is-upper,
  string-is-lower,
  string-is-alpha,
  string-is-whitespace,
  string-is-digit,
  string-is-punct,
  string-is-non-word-char,
  string-is-all-upper,
  string-remove-all,
  string-format
end

import lists as L

fun string-is-upper(character :: String) -> Boolean:
  doc: 'Is the one-character string an ASCII uppercase letter?'
  ("A" <= character) and (character <= "Z")
where:
  string-is-upper("A") is true
  string-is-upper("a") is false
  string-is-upper(" ") is false
  string-is-upper("-") is false
  string-is-upper("_") is false
  string-is-upper("'") is false
end

fun string-is-lower(character :: String) -> Boolean:
  doc: 'Is the one-character string an ASCII lowercase letter?'
  ("a" <= character) and (character <= "z")
where:
  string-is-lower("A") is false
  string-is-lower("a") is true
  string-is-lower(" ") is false
  string-is-lower("-") is false
  string-is-lower("_") is false
  string-is-lower("'") is false
end

fun string-is-alpha(character :: String) -> Boolean:
  doc: 'Is the one-character string an ASCII letter?'
  string-is-upper(character) or string-is-lower(character) 
where:
  string-is-alpha("A") is true
  string-is-alpha("a") is true
  string-is-alpha(" ") is false
  string-is-alpha("-") is false
  string-is-alpha("_") is false
  string-is-alpha("'") is false
end

fun string-is-whitespace(c):
  doc: "Return true if the one-character string c is a whitespace character."
  cp = string-to-code-point(c)
  [list: 8, 9, 10, 11, 13, 32].member(cp)
end

fun string-is-digit(character :: String) -> Boolean:
  doc: 'Is the one-character string a digit?'
  ("0" <= character) and (character <= "9")
where:
  string-is-digit("A") is false
  string-is-digit("0") is true
end

fun string-is-punct(character :: String) -> Boolean:
  doc: 'Is the one-character string an ASCII punctuation character?'
  (("!" <= character) and (character <= "/")) or
  ((":" <= character) and (character <= "@")) or
  (("[" <= character) and (character <= "`")) or
  (("{" <= character) and (character <= "~"))
end

# for `acronym` exercise
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

fun string-is-all-upper(s :: String) -> Boolean:
  doc: "Return true if the string s contains an uppercase letter but no lowercase letters."
  chars = string-explode(s)
  chars.any(string-is-upper) and
    chars.all(lam(c): not(string-is-lower(c)) end)
where:
  string-is-all-upper("HELLO") is true
  string-is-all-upper("HeLLO") is false
  string-is-all-upper("hello") is false
  string-is-all-upper("¡HELLO!") is true
  string-is-all-upper("1234?!") is false
end

#|  turns out this is builtin (but not documented?)
fun string-ends-with(s, t):
  doc: "Returns true if the string s ends with the string t."
  slen = string-length(s)
  tlen = string-length(t)
  ask:
    | tlen > slen then: false
    | otherwise: string-substring(s, slen - tlen, slen) == t
  end
end
|#


fun string-remove-all(s, cs):
  doc: "Remove all the characters in list cs from string s"
  string-explode(s)
  ^ filter({(c): not(cs.member(c))}, _)
  ^ L.join-str(_, '')
where:
  string-remove-all('hello', [list: 'e', 'l']) is 'ho'
  string-remove-all('hello', [list: '-']) is 'hello'
end

fun string-format(format-string, strings):
  doc: "..."
  for L.fold_n(i from 1, fmt from format-string, str from strings):
    string-replace(fmt, '{' + num-to-string(i) + '}', str)
  end
where:
  string-format('Hello, {1}!', [list: 'World']) is 'Hello, World!'
  string-format('He{1}{1}{2}, W{2}r{1}d!', [list: 'l', 'o']) is 'Hello, World!'
end
