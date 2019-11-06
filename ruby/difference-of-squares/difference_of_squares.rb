class Squares
  private

  attr_reader :num

  public

  def initialize(number)
    @num = number
  end

  def sum_of_squares
    1.upto(num).map { |n| n**2 }.reduce(:+)
  end

  def square_of_sum
    1.upto(num).reduce(:+)**2
  end

  def difference
    square_of_sum - sum_of_squares
  end
end
