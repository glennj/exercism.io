stringx = require 'pl.stringx'
List = require 'pl.List'
seq = require 'pl.seq'

-- ---------------------------------------------------------
class Card
  new: (@string) =>
    face, @suit = @string\match '(.+)([HDCS])'
    @value = switch face 
      when 'A' then 14
      when 'K' then 13
      when 'Q' then 12
      when 'J' then 11
      else tonumber face

  __lt: (other) => @value < other.value
  __eq: (other) => @value == other.value

-- ---------------------------------------------------------
-- Calculate the value of a list of cards, as a base-14 number
sum = (cards) ->
  accum = (acc, card) -> acc * 14 + card.value
  seq.reduce accum, cards, 0

-- ---------------------------------------------------------
class Hand
  new: (@string) =>
    @cards = [Card card for card in *(stringx.split @string)]
    table.sort @cards, (a, b) -> b < a  -- sort descending by face value
    @analyze!
    
  analyze: =>
    return if @fiveOfAKind!     -- 1
    return if @straightFlush!   -- 2, 3
    return if @fourOfAKind!     -- 4
    return if @fullHouse!       -- 5
    return if @flush!           -- 6
    return if @straight!        -- 7
    return if @threeOfAKind!    -- 8
    return if @twoPair!         -- 9
    return if @onePair!         -- 10
    @ranking = 11
    @value = sum @cards

  fiveOfAKind: =>
    return false if @cards[1] != @cards[5]
    @ranking = 1
    @value = @cards[1].value
    true
  
  straightFlush: =>
    return false unless @isFlush! and @isStraight!
    @ranking = if @cards[1].value == 14 and @cards[2].value == 13 then 2 else 3
    @value = @cards[1].value
    true

  fourOfAKind: =>
    if @cards[1] == @cards[4]
      @value = sum {@cards[1], @cards[5]}
    elseif @cards[2] == @cards[5]
      @value = sum {@cards[2], @cards[1]}
    else
      return false
    @ranking = 4
    true
  
  fullHouse: =>
    if @cards[1] == @cards[2] and @cards[3] == @cards[5]
      @value = sum {@cards[3], @cards[1]}
    elseif @cards[1] == @cards[3] and @cards[4] == @cards[5]
      @value = sum {@cards[1], @cards[4]}
    else
      return false
    @ranking = 5
    true

  flush: =>
    return false unless @isFlush!
    @ranking = 6
    @value = sum @cards
    true

  straight: =>
    return false unless @isStraight!
    @ranking = 7
    @value = @cards[1].value
    true

  threeOfAKind: =>
    if @cards[1] == @cards[3]
      @value = sum {@cards[1], @cards[4], @cards[5]}
    elseif @cards[2] == @cards[4]
      @value = sum {@cards[2], @cards[1], @cards[5]}
    elseif @cards[3] == @cards[5]
      @value = sum {@cards[3], @cards[1], @cards[2]}
    else
      return false
    @ranking = 8
    true

  twoPair: =>
    if @cards[1] == @cards[2] and @cards[3] == @cards[4]
      @value = sum {@cards[1], @cards[3], @cards[5]}
    elseif @cards[1] == @cards[2] and @cards[4] == @cards[5]
      @value = sum {@cards[1], @cards[4], @cards[3]}
    elseif @cards[2] == @cards[3] and @cards[4] == @cards[5]
      @value = sum {@cards[2], @cards[4], @cards[1]}
    else
      return false
    @ranking = 9
    true

  onePair: =>
    if @cards[1] == @cards[2]
      @value = sum {@cards[1], @cards[3], @cards[4], @cards[5]}
    elseif @cards[2] == @cards[3]
      @value = sum {@cards[2], @cards[1], @cards[4], @cards[5]}
    elseif @cards[3] == @cards[4]
      @value = sum {@cards[3], @cards[1], @cards[2], @cards[5]}
    elseif @cards[4] == @cards[5]
      @value = sum {@cards[4], @cards[1], @cards[2], @cards[3]}
    else
      return false
    @ranking = 10
    true

  isFlush: =>
    #@cards == #[card for card in *@cards when card.suit == @cards[1].suit]

  isStraight: =>
    values = List [card.value for card in *@cards]
    if values == {14, 5, 4, 3, 2} -- ace-low straight
      table.insert @cards, (table.remove @cards, 1)
      return true
    for i = 2, #values
      return false if values[i - 1] - 1 != values[i]
    true

  __lt: (other) =>
    return true if @ranking < other.ranking
    return true if @ranking == other.ranking and @value > other.value
    false

  __eq: (other) => @ranking == other.ranking and @value == other.value
    

-- ---------------------------------------------------------
{
  bestHands: (handStrings) ->
    hands = [Hand h for h in *handStrings]
    table.sort hands
    [hand.string for hand in *hands when hand == hands[1]]
}
