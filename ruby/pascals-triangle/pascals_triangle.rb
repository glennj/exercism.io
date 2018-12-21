require 'memoist'

class Triangle
  extend Memoist

  attr_reader :rows

  def initialize(size)
    @rows = Array.new(size) { |i| row(i) }
  end

  private

  def row(n)
    (0..n).map { |k| binom(n, k) }
  end

  # binomial coefficient, or "n choose k"
  def binom(n, k)
    return 1 if k.zero? || (n - k).zero?
    return n if k == 1 || n - k == 1

    factorial(n) / (factorial(k) * factorial(n - k))
  end

  def factorial(n)
    return 1 if n == 1

    n * factorial(n - 1)
  end
  memoize :factorial
end
