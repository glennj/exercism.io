class Matrix
  def initialize(input)
    @input = input
  end

  def rows
    @rows ||= @input.split("\n").map { |row| row.split(' ').map(&:to_i) }
  end

  def columns
    @columns ||= rows.transpose
  end

  def row(n)
    rows[n - 1]
  end

  def column(n)
    columns[n - 1]
  end
end
