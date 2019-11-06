class Triplet
  private

  attr_reader :sides

  public

  def initialize(*sides)
    @sides = sides.sort
    raise ArgumentError if @sides.size != 3  ||
                           @sides.first <= 0 ||
                           @sides.take(2).sum <= @sides.last
  end

  def sum
    sides.reduce(&:+)
  end

  def product
    sides.reduce(&:*)
  end

  def pythagorean?
    sides[0]**2 + sides[1]**2 == sides[2]**2
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
