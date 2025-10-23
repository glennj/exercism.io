use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide:
  format-message
end

fun format-message(person, position):
  person + ", you are the " + ordinal(position) + " customer we serve today. Thank you!"
end

fun ordinal(number):
  ones = num-modulo(number, 10)
  tens = num-modulo(number, 100)
  num-to-string(number)
    + ask:
      | (ones == 1) and (tens <> 11) then: "st"
      | (ones == 2) and (tens <> 12) then: "nd"
      | (ones == 3) and (tens <> 13) then: "rd"
      | otherwise: "th"
    end
end
