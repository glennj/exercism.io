module AllYourBase
  def self.rebase(input_base : Int32, digits : Array(Int32), output_base : Int32) : Array(Int32)
    raise ArgumentError.new if input_base < 2 \
                            || output_base < 2 \
                            || digits.any? {|d| d.negative? || d >= input_base}

    decimal = digits.reduce(0) {|sum, d| sum * input_base + d}
    to_digits = [] of Int32
    loop do
      decimal, rem = decimal.divmod(output_base)
      to_digits.unshift(rem)
      break if decimal.zero?
    end
    to_digits
  end
end
