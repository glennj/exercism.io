module Diamond
  module_function

  def make_diamond(char)
    char.upcase!
    north = northern_hemisphere(char)
    north.concat(north.reverse[1..-1]).join('')
  end

  def northern_hemisphere(char)
    half_line = ' ' * (char.ord - 'A'.ord + 1)
    ('A'..char)
      .each_with_index
      .each_with_object([]) do |(c, i), north|
        half = half_line.dup
        half[i] = c
        north << half[1..-1].reverse + half + "\n"
      end
  end
end
