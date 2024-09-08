class Yacht
  @score: (dice, category) -> new this(dice).score(category)

  constructor: (dice) ->
    @dice = dice.slice().sort((a, b) -> a - b)
    @sum = dice.reduce((sum, d) -> sum + d)

  score: (category) ->    
    switch category
      when 'ones' then @single(1)
      when 'twos' then @single(2)
      when 'threes' then @single(3)
      when 'fours' then @single(4)
      when 'fives' then @single(5)
      when 'sixes' then @single(6)
      when 'full house' then @fullHouse()
      when 'four of a kind' then @fourOfAKind()
      when 'little straight' then @straight([1..5])
      when 'big straight' then @straight([2..6])
      when 'yacht' then @yacht()
      when 'choice' then @sum
      else 0

  single: (target) ->
    target * (d for d in @dice when d == target).length
    
  fourOfAKind: () ->
    if (@dice.first() == @dice.fourth()) or (@dice.second() == @dice.fifth())
      @dice.second() * 4
    else
      0

  fullHouse: () ->
    switch
      when @dice.first() == @dice.fifth() then 0
      when @dice.first() == @dice.second() and @dice.third() == @dice.fifth() then @sum
      when @dice.first() == @dice.third() and @dice.fourth() == @dice.fifth() then @sum
      else 0

  straight: (target) -> 
    if @dice.every((d, i) -> d == target[i]) then 30 else 0
    
  yacht: () -> 
    if @dice.first() == @dice.fifth() then 50 else 0

      
Array.prototype.first = () -> this[0]
Array.prototype.second = () -> this[1]
Array.prototype.third = () -> this[2]
Array.prototype.fourth = () -> this[3]
Array.prototype.fifth = () -> this[4]


module.exports = Yacht
