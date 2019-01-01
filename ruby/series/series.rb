class Series
  def initialize(input)
    @chars = input.chars
  end

  def slices(size)
    raise ArgumentError if size > @chars.size

    @chars.each_cons(size).map(&:join)
  end
end
