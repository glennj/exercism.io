use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: clean end

import file("string-helpers.arr") as SH
include from SH:
  string-remove-all,
  string-is-alpha,
  string-is-punct
end
import lists as L

fun clean(phone-number):
  # remove valid non-digits
  a = string-remove-all(phone-number, [list: '(', ')', ' ', '-', '.'])

  # validating length
  b = string-explode(
        ask:
          | string-length(a) < 10 then: raise("must not be fewer than 10 digits")
          | string-length(a) == 10 then: a
          | string-length(a) == 11 then:
              ask:
                | string-starts-with(a, "1") then: string-substring(a, 1, 11)
                | otherwise: raise("11 digits must start with 1")
              end
          | (string-length(a) == 12) and string-starts-with(a, "+1") then: string-substring(a, 2, 12)
          | otherwise: raise("must not be greater than 11 digits")
        end
      )

  # validating content
  ask:
    | b.any({(x): string-is-alpha(x)}) then: raise("letters not permitted")
    | b.any({(x): string-is-punct(x)}) then: raise("punctuations not permitted")
    | b.get(0) == "0" then: raise("area code cannot start with zero")
    | b.get(0) == "1" then: raise("area code cannot start with one")
    | b.get(3) == "0" then: raise("exchange code cannot start with zero")
    | b.get(3) == "1" then: raise("exchange code cannot start with one")
    | otherwise: L.join-str(b, '')
  end
end
