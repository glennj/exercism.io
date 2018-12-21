class Triplet
  def initialize(*sides)
    @a, @b, @c = sides.sort
    raise ArgumentError if @a <= 0 || @a + @b <= @c
  end

  def sum
    [@a, @b, @c].reduce(&:+)
  end

  def product
    [@a, @b, @c].reduce(&:*)
  end

  def pythagorean?
    @a**2 + @b**2 == @c**2
  end

  def self.where(max_factor: 1, min_factor: 1, sum: nil)
    result = []
    max_factor.downto(min_factor) do |c|
      (c - 1).downto(min_factor) do |b|
        a = Math.sqrt(c**2 - b**2)
        next unless a > b && a == a.to_i

        t = new(a, b, c)
        result << t if t.pythagorean? && (sum.nil? || sum == t.sum)
      end
    end
    result
  end
end
