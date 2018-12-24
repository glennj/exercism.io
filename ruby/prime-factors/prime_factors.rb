require_relative '../nth-prime/nth_prime'

module PrimeFactors
  module_function

  def for(number)
    factors = []
    f = Prime.nth(i = 1)
    while f * f <= number
      if (number % f).zero?
        factors << f
        number /= f
      else
        f = Prime.nth(i += 1)
      end
    end
    factors << number if number > 1
    factors
  end
end

# or, using the 'prime' module
#
# require 'prime'
#
# module PrimeFactors
#   module_function
#
#   def for(number)
#     Prime.prime_division(number)
#          .map { |(factor, count)| [factor] * count }
#          .flatten
#   end
# end
