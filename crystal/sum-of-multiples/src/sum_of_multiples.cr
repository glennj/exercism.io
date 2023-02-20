module SumOfMultiples
  def self.sum(factors : Array(Number), limit : Number) : Number
    fs = factors.reject(&.zero?)
    (1...limit)
      .select {|n| fs.any? {|f| n.divisible_by? f}}
      .sum
  end
end
