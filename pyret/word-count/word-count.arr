use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: word-count end

include string-dict
include file("string-helpers.arr")

fun word-count(phrase) block:
  words = parse(phrase)
  counts = [mutable-string-dict: ]
  for each(word from words):
    cases(Option) counts.get-now(word):
      | none => counts.set-now(word, 1)
      | some(n) => counts.set-now(word, n + 1)
    end
  end
  counts.freeze()
end

fun parse(phrase):
  doc: "Extract the words from the phrase"
  rec parser = lam(state, chars, word, words):
    cases(List) chars:
      | empty => 
          trimmed = string-trim(word, "'")
          if string-length(trimmed) == 0:
            words
          else:
            words + [list: trimmed]
          end
      | link(c, cs) =>
          if state == 'seeking':
            # Looking for the start of a word
            if string-is-non-word-char(c):
              parser(state, cs, word, words)
            else:
              parser('in-word', cs, string-to-lower(c), words)
            end
          else:
            # In a word
            if not(string-is-non-word-char(c)):
              parser(state, cs, word + string-to-lower(c), words)
            else:
              trimmed = string-trim(word, "'")
              if string-length(trimmed) == 0:
                parser('seeking', cs, '', words)
              else:
                parser('seeking', cs, '', words + [list: trimmed])
              end
            end
          end
    end
  end
  parser('seeking', string-explode(phrase), '', [list:])
end
