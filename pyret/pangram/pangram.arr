use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: is-pangram end

include file("string-helpers.arr")
import lists as L

fun is-pangram(phrase):
  ( phrase
      ^ string-to-upper(_)
      ^ string-explode(_)
      ^ filter(string-is-upper, _)
      ^ L.distinct(_)
      ^ L.length(_)
  ) == 26
end
