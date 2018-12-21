module IsbnVerifier
  module_function

  def valid?(input)
    isbn = input.delete('-')

    # 9 digits followed by a digit or 'X'
    return false unless isbn =~ /^\d{9}[\dX]$/

    chars = isbn.chars
    check = chars.pop
    digits = chars.map(&:to_i)
    digits << (check == 'X' ? 10 : check.to_i)

    digits.each_with_index
          .reduce(0) { |sum, (d, i)| sum + d * (10 - i) }
          .modulo(11)
          .zero?
  end
end
