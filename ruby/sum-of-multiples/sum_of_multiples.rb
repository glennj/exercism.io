class SumOfMultiples
  private

  attr_reader :factors

  public

  def initialize(*factors)
    @factors = factors
  end

  def to(limit)
    1 .upto(limit - 1)
      .select { |n| factors.any? { |f| (n % f).zero? } }
      .sum
  end
end
