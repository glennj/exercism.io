class String
  # right-pad a string with spaces
  def rpad!(len, fill = ' ')
    self << (fill * len)[length..-1] if length < len
  end
  # lpad! and non-mutating versions left as an exercise
end

module Transpose
  module_function

  def transpose(input)
    return '' if input.empty?

    lines = pad_descending(input.lines.map(&:chomp))
    # now, the transposition
    rows = lines.map(&:chars)
    cols = Array.new(rows[0].length) do |i|
      rows.map { |row| row[i] }
    end
    cols.map { |col| col.join('') }.join("\n")
  end

  # Pad an array of lines such that each line
  # is at least as long as any following lines.
  #
  # example: ["foo", "bang", "barbaz", "qux"]
  # becomes: ["foo   ", "bang  ", "barbaz", "qux"]
  # because line 2 is longer
  #
  def pad_descending(lines)
    # starting from the bottom...
    max = lines[-1].length
    # ... gradually increase the max length as longer lines are encountered.
    (lines.length - 2).downto(0) do |i|
      len = lines[i].length
      if len < max
        lines[i].rpad!(max)
      else
        max = len
      end
    end
    lines
  end
end
