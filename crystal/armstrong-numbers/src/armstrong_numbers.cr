module ArmstrongNumbers
  def self.armstrong_number?(input : Number) : Bool
    digits = input.digits
    armstrong_sum = digits.map {|d| d ** digits.size}.sum
    armstrong_sum == input
  end
end
