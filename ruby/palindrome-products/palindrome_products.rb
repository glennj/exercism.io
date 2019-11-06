module PalindromeTest
  refine Integer do
    def palindrome?
      s = to_s
      s == s.reverse
    end
  end
end

class Palindromes
  using PalindromeTest

  Palindrome = Struct.new(:value, :factors)

  attr_reader :largest, :smallest

  def initialize(**options)
    @min = options[:min_factor] || 1
    @max = options[:max_factor]
    raise ArgumentError, 'No max factor' unless @max
  end

  def generate
    factors = Hash.new { |h, k| h[k] = [] }

    (@min..@max).each do |i|
      (i..@max).each do |j|
        factors[i * j] << [i, j]
      end
    end

    pals = factors.keys.select(&:palindrome?).sort
    @largest = Palindrome.new(pals.last, factors[pals.last])
    @smallest = Palindrome.new(pals.first, factors[pals.first])
  end
end
