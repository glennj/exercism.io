module BaseConverter
  module_function

  def convert(from, digits, to)
    raise ArgumentError, "bad input base #{from}" if from <= 1
    raise ArgumentError, "bad output base #{to}" if to <= 1
    raise ArgumentError, 'bad digits' if digits.any? { |d| d < 0 || d >= from }

    decimal = digits.reduce(0) { |s, d| s * from + d }
    to_digits = []
    loop do
      decimal, rem = decimal.divmod(to)
      to_digits.unshift(rem)
      break if decimal.zero?
    end
    to_digits
  end
end
