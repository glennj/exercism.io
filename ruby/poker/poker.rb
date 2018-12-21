class Poker
  def initialize(hands)
    @hands = hands.map { |h| PokerHand.new(h) }
  end

  def best_hand
    grouped = @hands.group_by(&:value)
    max = grouped.keys.max
    best = grouped[max]
    if best.length > 1
      grouped = best.group_by(&:cards_ranked)
      max = grouped.keys.max
      best = grouped[max]
    end
    best.map(&:original)
  end
end

class PokerHand
  attr_reader :value, :cards_ranked, :original

  def initialize(cards)
    raise ArgumentError unless cards.length == 5

    @original = cards
    @cards = cards.map { |c| Card.new c }.sort_by(&:value)
    @histogram = histogram
    @value = evaluate
  end

  private

  # these methods return the "value" of a hand,
  # where 5 of a kind is the highest,
  # and also set the @cards_ranked instance variable
  # which is needed to break ties.

  def evaluate
    five_of_a_kind?  ||
    straight_flush?  ||
    four_of_a_kind?  ||
    full_house?      ||
    flush?           ||
    straight?        ||
    three_of_a_kind? ||
    two_pair?        ||
    one_pair?        ||
    high_card
  end

  public

  def five_of_a_kind?
    if @histogram.length == 1
      @cards_ranked = [@cards.first.value]
      return 9
    end
    nil
  end

  def flush?
    suit = @cards.last.suit
    if @cards.all? { |c| c.suit == suit }
      @cards_ranked = @cards.map(&:value).reverse
      return 5
    end
    nil
  end

  def straight?
    if @cards.each_cons(2).all? { |a, b| b.value - a.value == 1 }
      @cards_ranked = [@cards.last.value]
      return 4
    end
    # or do we have 5-high straight?
    if @cards.map(&:value) == [0, 1, 2, 3, 12]
      @cards_ranked = [3] # face value is 5
      return 4
    end
    nil
  end

  def straight_flush?
    return 8 if flush? && straight?
    @cards_ranked = nil # in case flush but not straight
    nil
  end

  def four_of_a_kind?
    by_histogram [1, 4], 7
  end

  def full_house?
    by_histogram [2, 3], 6
  end

  def three_of_a_kind?
    by_histogram [1, 1, 3], 3
  end

  def two_pair?
    by_histogram [1, 2, 2], 2
  end

  def one_pair?
    by_histogram [1, 1, 1, 2], 1
  end

  def high_card
    @cards_ranked = @cards.map(&:value).reverse
    0
  end

  def <=>(other)
    value <=> other.value || cards_ranked <=> other.cards_ranked
  end

  def to_s
    @cards.map(&:to_s)
  end

  private

  def histogram
    @cards
      .map(&:value)
      .group_by { |v| v }
      .map { |k, v| [k, v.size] }
      .sort { |a, b| a.last <=> b.last || a.first <=> b.first }
  end

  def by_histogram(grouping, hand_value)
    if @histogram.map(&:last) == grouping
      @cards_ranked = @histogram.map(&:first).reverse
      return hand_value
    end
    nil
  end
end

class Card
  CARD_FACES = (2..10).map(&:to_s).concat(%w[J Q K A]).freeze

  attr_reader :value, :suit

  def initialize(input)
    @value = nil
    @suit = nil
    input.match(/^([1-9]|10|[JQKA])([CSHD])$/) do |m|
      @value = CARD_FACES.index m[1]
      @suit = m[2]
    end
    raise ArgumentError, "invalid card #{input}" if @value.nil?
  end

  def to_s
    "#{CARD_FACES[value]}#{suit}"
  end
end
