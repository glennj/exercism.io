module PrimeFactors
  def self.factors(value : Int64) : Array(Int32)
    factors = [] of Int32
    f = 2
    while f * f <= value
      if value.divisible_by? f
        factors << f
        value //= f
      else
        f += 1
      end
    end
    factors << value.to_i32 if value > 1
    factors
  end
end
