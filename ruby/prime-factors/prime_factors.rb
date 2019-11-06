module PrimeFactors
  module_function

  def of(number)
    factors = []
    f = 2
    while f * f <= number
      if (number % f).zero?
        factors << f
        number /= f
      else
        f += 1
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
