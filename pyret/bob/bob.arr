use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: response end

include file("string-helpers.arr")

fun response(hey-bob):
  input = string-remove-whitespace(hey-bob)

  is-silent = string-length(input) == 0
  is-question = string-ends-with(input, "?")
  is-yelling = string-is-all-upper(input)

  ask:
    | is-silent then: "Fine. Be that way!"
    | is-yelling and is-question then: "Calm down, I know what I'm doing!"
    | is-yelling then: "Whoa, chill out!"
    | is-question then: "Sure."
    | otherwise: "Whatever."
  end
end

fun string-remove-whitespace(s):
  string-explode(s)
    .filter(lam(c): not(string-is-whitespace(c)) end)
    .join-str("")
end
