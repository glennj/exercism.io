use context essentials2020

provide: 
  data HighScores
end

data HighScores: high-scores(scores :: List<NumInteger>)
with:
  method latest(self) -> NumInteger:
    self.scores.last()
  end,

  method personal-best(self) -> NumInteger:
    self.scores.foldl({(score, max): num-max(max, score)}, 0)
  end,

  method personal-top-three(self) -> List<NumInteger>:
    sorted = self.scores.sort-by({(a, b): a > b}, {(a, b): a == b})
    sorted.take(num-min(3, sorted.length()))
  end
end
