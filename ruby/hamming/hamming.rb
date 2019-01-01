module Hamming
  module_function

  def compute(a, b)
    raise ArgumentError if a.length != b.length
    a.chars
      .zip( b.chars )
      .count {|(c1, c2)| c1 != c2}
  end
end