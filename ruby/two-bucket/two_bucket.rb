class Bucket
  attr_accessor :amount
  attr_reader :name, :size

  def initialize(name, size)
    @name = name
    @size = size
    empty
  end

  def capacity
    size - amount
  end

  def fill
    self.amount = size
  end

  def empty
    self.amount = 0
  end

  def full?
    amount == size
  end

  def empty?
    amount.zero?
  end

  def pour_into(other)
    to_pour = [amount, other.capacity].min
    self.amount -= to_pour
    other.amount += to_pour
  end
end

class TwoBucket
  attr_reader :moves, :goal_bucket, :other_bucket

  private

  attr_writer :moves

  public

  def initialize(size1, size2, goal, start_name)
    validate(size1, size2, goal, start_name)

    one = Bucket.new('one', size1)
    two = Bucket.new('two', size2)
    start, other = start_name == 'one' ? [one, two] : [two, one]

    @goal_bucket, @other_bucket = solve start, other, goal
  end

  private

  def solve(start, other, goal)
    start.fill
    self.moves = 1

    loop do
      return [start.name, other.amount] if start.amount == goal
      return [other.name, start.amount] if other.amount == goal

      if other.size == goal then other.fill
      elsif start.empty?    then start.fill
      elsif other.full?     then other.empty
      else                       start.pour_into(other)
      end

      self.moves += 1
    end
  end

  def validate(size1, size2, goal, start_name)
    gcd = size1.gcd(size2)
    raise ArgumentError if goal > [size1, size2].max
    raise ArgumentError unless gcd == 1 || (goal % gcd).zero?
    raise ArgumentError unless %w[one two].include? start_name
  end
end
