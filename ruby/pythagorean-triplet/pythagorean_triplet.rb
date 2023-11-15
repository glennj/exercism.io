module PythagoreanTriplet
  def triplets_with_sum(perimeter)
    triplets = []
    a = 0
    while true do
      a += 1
      num = perimeter * (perimeter - 2 * a)
      den = 2 * (perimeter - a)
      b = num / den
      break if b < a
      next if num % den != 0
      c = perimeter - a - b
      triplets << [a, b, c]
    end
    return triplets
  end
  module_function :triplets_with_sum
end
