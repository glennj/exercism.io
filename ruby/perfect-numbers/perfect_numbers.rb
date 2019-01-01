module PerfectNumber
  module_function

  def classify(num)
    sum = aliquot_sum num
    return 'perfect' if sum == num
    return 'deficient' if sum < num
    'abundant'
  end

  def aliquot_sum(num)
    raise 'invalid' if num < 1

    factors = [1]
    sqrt = Math.sqrt(num).floor
    2.upto(sqrt) do |factor|
      div, rem = num.divmod(factor)
      factors << factor << div if rem.zero?
    end
    factors.sum
  end
end
