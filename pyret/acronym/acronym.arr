use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: abbreviate end

include file("char-helpers.arr")

fun abbreviate(phrase):
  fun __abbreviate(phr, state, acronym):
    if phr == "":
      acronym
    else:
      character = string-char-at(phr, 0)
      rest = string-substring(phr, 1, string-length(phr))
      ask:
        | (state == "seeking-alpha") and char-is-alpha(character) then:
            __abbreviate(rest, "seeking-end-of-word", acronym + character)
        | (state == "seeking-end-of-word") and char-is-non-word-char(character) then:
            __abbreviate(rest, "seeking-alpha", acronym)
        | otherwise:
            __abbreviate(rest, state, acronym)
      end
    end
  end

  string-to-upper(__abbreviate(phrase, "seeking-alpha", ""))
end
