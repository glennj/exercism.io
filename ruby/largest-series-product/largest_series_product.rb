class Series
  def initialize(input)
    raise ArgumentError if input =~ /\D/

    @series = input.chars.map(&:to_i)
  end

  def largest_product(size)
    return 1 if size.zero?
    raise ArgumentError if @series.empty? || size > @series.length

    @series.each_cons(size)
           .map { |digits| digits.reduce(&:*) }
           .max
  end
end
