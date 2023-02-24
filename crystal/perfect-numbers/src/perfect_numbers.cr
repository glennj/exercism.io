module PerfectNumbers
  def self.aliquotSum(num : Number) : Number
    raise ArgumentError.new unless num.positive?
    1.upto(Math.isqrt num)
     .select {|i| num.divisible_by? i}
     .flat_map {|i| [i, num // i]}
     .to_set
     .subtract([num])
     .sum
  end

  def self.classify(num : Number) : String
    sum = aliquotSum num
    case
      when sum < num then "deficient"
      when sum > num then "abundant"
      else "perfect"
    end
  end
end
