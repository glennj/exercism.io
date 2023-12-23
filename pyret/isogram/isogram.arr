use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: is-isogram end

include file("string-helpers.arr")

fun is-isogram(phrase):
  rec checker = lam(letters, seen):
    cases(List) letters:
      | empty => true
      | link(char, rest) =>
          ask:
            | not(string-is-alpha(char)) then: checker(rest, seen)
            | not(seen.member(char)) then: checker(rest, seen.add(char))
            | otherwise: false
          end
    end
  end

  checker(phrase ^ string-to-lower(_) ^ string-explode(_), [set:])
end
