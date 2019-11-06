module ArmstrongNumbers
  module_function

  def include?(num)
    digits = num.digits
    armstrong_sum = digits.map { |d| d**digits.length }.sum
    armstrong_sum == num
  end
end
