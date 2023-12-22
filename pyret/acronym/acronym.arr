use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: abbreviate end

include file("string-helpers.arr")

fun abbreviate(phrase):
  fun abbr(phr, state, acronym):
    if phr == "":
      acronym
    else:
      character = string-char-at(phr, 0)
      rest = string-substring(phr, 1, string-length(phr))
      ask:
        | (state == "seeking-alpha") and string-is-alpha(character) then:
            abbr(rest, "seeking-end-of-word", acronym + character)
        | (state == "seeking-end-of-word") and string-is-non-word-char(character) then:
            abbr(rest, "seeking-alpha", acronym)
        | otherwise:
            abbr(rest, state, acronym)
      end
    end
  end

  phrase
  ^ abbr(_, "seeking-alpha", "")
  ^ string-to-upper(_)
end
