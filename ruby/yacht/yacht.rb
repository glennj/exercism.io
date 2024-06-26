class Yacht
  attr_reader :dice, :category
  private attr_reader :count

  def initialize(dice, category)
    @dice = dice.sort.freeze
    @category = category.gsub(' ', '_').to_sym
    @count = @dice.tally
    @count.default = 0
  end

  def score
    case @category
    when :ones            then score_for 1
    when :twos            then score_for 2
    when :threes          then score_for 3
    when :fours           then score_for 4
    when :fives           then score_for 5
    when :sixes           then score_for 6
    when :little_straight then straight 1..5
    when :big_straight    then straight 2..6
    else self.send @category
    end
  end

  private

  def score_for(die)
    die * @count[die]
  end

  def full_house
    @count.values.sort == [2,3] ? choice : 0
  end

  def four_of_a_kind
    (@count.filter_map {|d, c| d if c >= 4}.first || 0) * 4
  end

  def straight(range)
    @dice == range.to_a ? 30 : 0
  end

  def yacht
    @count.size == 1 ? 50 : 0
  end

  def choice
    @dice.sum
  end
end
