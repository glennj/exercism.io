module Squares
  extend self

  def square_of_sum(n : Int) : Int
    (1.upto(n).sum) ** 2
  end

  def sum_of_squares(n : Int) : Int
    1.upto(n).map(&.** 2).sum
  end

  def difference_of_squares(n : Int) : Int
    (sum_of_squares(n) - square_of_sum(n)).abs
  end
end
