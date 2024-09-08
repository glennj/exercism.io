class Triangle
  constructor: (a, b, c) -> 
    [@a, @b, @c] = [a, b, c].sort((a, b) -> a - b)
    @valid = @a > 0 and @a + @b >= @c

  equilateral: -> @valid and @a == @b and @a == @c
  isosceles: -> @valid and (@a == @b or @a == @c or @b == @c)
  scalene: -> @valid and not @isosceles()

module.exports = Triangle
