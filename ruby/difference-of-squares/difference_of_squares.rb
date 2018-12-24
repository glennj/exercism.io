class Squares
  def initialize(n)
    @sum_of_squares = 1.upto(n).map {|m| m ** 2}.reduce(:+)
    @square_of_sum  = 1.upto(n).reduce(:+) ** 2
    @difference = @square_of_sum - @sum_of_squares
  end
  attr_reader :sum_of_squares, :square_of_sum, :difference
end
