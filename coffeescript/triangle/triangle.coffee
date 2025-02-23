class Triangle
  constructor: (a, b, c) -> 
    [@a, @b, @c] = [a, b, c].sort((a, b) -> a - b)
    @valid = @a > 0 and @a + @b >= @c

  # we don't need to check every pair of sides since they're sorted
  equilateral: -> @valid and @a == @c
  isosceles: -> @valid and (@a == @b or @b == @c)
  scalene: -> @valid and not @isosceles()

module.exports = Triangle
