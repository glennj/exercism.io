use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: equilateral, isosceles, scalene end

import lists as L

fun num-distinct-sides(sides):
  L.distinct(sides).length()
end

fun valid-triangle(sides):
  sorted = sides.sort()
  (sorted.get(0) >= 0) and ((sorted.get(0) + sorted.get(1)) > sorted.get(2))
end

fun equilateral(sides):
  valid-triangle(sides) and (num-distinct-sides(sides) == 1)
end

fun isosceles(sides):
  valid-triangle(sides) and (num-distinct-sides(sides) < 3)
end

fun scalene(sides):
  valid-triangle(sides) and (num-distinct-sides(sides) == 3)
end
