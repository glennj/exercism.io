module Blackjack
  def self.parse_card(card)
    case card
    when "ace"   then 11
    when "king", "queen", "jack", "ten" then 10
    when "nine"  then 9
    when "eight" then 8
    when "seven" then 7
    when "six"   then 6
    when "five"  then 5
    when "four"  then 4
    when "three" then 3
    when "two"   then 2
    else              0
    end
  end

  def self.card_range(card1, card2)
    case parse_card(card1) + parse_card(card2)
    when     22 then "pair of aces"
    when     21 then "blackjack"
    when 17..20 then "high"
    when 12..16 then "mid"
    when  4..11 then "low"
    end
  end

  def self.first_turn(card1, card2, dealer_card)
    case card_range(card1, card2)
    when "pair of aces" then "P"
    when "blackjack"    then parse_card(dealer_card) < 10 ? "W" : "S"
    when "high"         then "S"
    when "mid"          then parse_card(dealer_card) <  7 ? "S" : "H"
    when "low"          then "H"
    end
  end
end
