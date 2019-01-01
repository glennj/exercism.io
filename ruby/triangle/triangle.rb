class Triangle
  def initialize(sides)
    @a, @b, @c = sides.sort
    @valid = @a > 0 && @a + @b > @c
  end

  def equilateral?
    @valid && @a == @b && @b == @c
  end

  def isosceles?
    @valid && (@a == @b || @a == @c || @b == @c)
  end

  def scalene?
    @valid && !equilateral? && !isosceles?
  end
end
