class Triangle
  private

  attr_reader :a, :b, :c, :valid

  public

  def initialize(sides)
    @a, @b, @c = sides.sort
    @valid = @a.positive? && (@a + @b - @c).positive?
  end

  def equilateral?
    valid && a == b && b == c
  end

  def isosceles?
    valid && (a == b || a == c || b == c)
  end

  def scalene?
    valid && !equilateral? && !isosceles?
  end
end
