class SumOfMultiples
  def initialize(*factors)
    @factors = factors
  end

  def to(limit)
    1.upto(limit - 1).select do |n|
      @factors.any? { |f| (n % f).zero? }
    end.sum
  end
end
