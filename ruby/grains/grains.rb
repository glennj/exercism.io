module Grains
  module_function

  def square(square)
    raise ArgumentError unless square.between? 1, 64
    2**(square - 1)
  end

  def total
    2**64 - 1
  end
end
