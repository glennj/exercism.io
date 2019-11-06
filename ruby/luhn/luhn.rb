# comment
module Luhn
  module_function

  def luhn_sum(digits)
    digits.reverse.each_with_index.reduce(0) do |sum, (n, i)|
      nn = n * ((i % 2).odd? ? 2 : 1)
      nn -= 9 if nn > 9
      sum + nn
    end
  end

  def valid?(input)
    return false if input =~ /[^[:digit:][:blank:]]/

    digits = input.gsub(/\D/, '').chars.map(&:to_i)
    return false if digits.length < 2

    (luhn_sum(digits) % 10).zero?
  end
end
