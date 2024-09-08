class ScrabbleScore
  @score: (word) -> [word.toUpperCase()...].map(@letterScore).sum()
    
  @letterScore: (letter) ->
    switch letter
      when 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' then 1
      when 'D', 'G' then 2
      when 'B', 'C', 'M', 'P' then 3
      when 'F', 'H', 'V', 'W', 'Y' then 4
      when 'K' then 5
      when 'J', 'X' then 8
      when 'Q', 'Z' then 10
      else 0
        
Array::sum = (initial = 0) -> this.reduce(((sum, elem) -> sum + elem), initial)

module.exports = ScrabbleScore
