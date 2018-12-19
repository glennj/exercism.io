module Grains
  module_function

  def square(n)
    raise ArgumentError unless n.between? 1, 64
    2 ** (n - 1)
  end
  
  def total
    2 ** 64 - 1

    # or
    # 1.upto(64).map {|n| self.square(n)}.reduce(:+)
  end
end
