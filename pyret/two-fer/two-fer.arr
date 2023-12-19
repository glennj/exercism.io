use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: two-fer end

fun two-fer(name):
  #|
    "One for "
    + if name == "": "you" else: name end
    + ", one for me."
  |#

  #|
    ask:
      | name == "" then: "One for you, one for me."
      | otherwise: "One for " + name + ", one for me."
    end
  |#

  name-opt = if name == "": none else: some(name) end
  "One for " + name-opt.or-else("you") + ", one for me."
end
