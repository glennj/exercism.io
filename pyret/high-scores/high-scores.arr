use context essentials2020

# provides a datatype, compare with matrix exercise
# that provides a function that returns an object.

provide: 
  data HighScores
end

import math as M

data HighScores: high-scores(scores :: List<NumInteger>)
with:
  method latest(self) -> NumInteger:
    self.scores.last()
  end,

  method personal-best(self) -> NumInteger:
    M.max(self.scores)
  end,

  method personal-top-three(self) -> List<NumInteger>:
    sorted = self.scores.sort-by({(a, b): a > b}, {(a, b): a == b})
    sorted.take(num-min(3, sorted.length()))
  end
end
