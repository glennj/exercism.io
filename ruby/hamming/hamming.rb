module Hamming
  module_function

  def compute(left, right)
    raise ArgumentError if left.length != right.length

    left
      .chars
      .zip(right.chars)
      .count { |(c1, c2)| c1 != c2 }
  end
end
