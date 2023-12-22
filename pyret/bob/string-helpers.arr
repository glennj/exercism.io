provide:
  string-is-upper,
  string-is-lower,
  string-is-alpha,
  string-is-whitespace,
  string-is-non-word-char,
  string-is-all-upper
end

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
    | otherwise: string-substring(s, slen-tlen, slen) == t
  end
end
|#
