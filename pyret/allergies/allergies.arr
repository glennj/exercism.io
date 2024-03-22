use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide-types *

include file("bitwise-helpers.arr")
include file("list-helpers.arr")

data Allergies:
  | allergies(score :: Number)
    with:

    method allergicTo(self, allergen :: String) -> Boolean:
      self.list().member(allergen)
    end,

    method list(self) -> List<String>:
      with-index([list: 'eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'])
        .filter(lam(pair): num-and1(num-shr(self.score, pair.{0})) == 1 end)
        .map(lam(pair): pair.{1} end)
    end
end
