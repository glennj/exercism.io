module Grains
  def self.square(number : Number) : Number
    raise ArgumentError.new unless (1..64).includes? number
    1_u64 << (number - 1)
  end

  def self.total : Number
    # UInt64::MAX
    (1_u128 << 64) - 1
  end
end
