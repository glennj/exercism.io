class Matrix
  private

  attr_reader :input

  public

  def initialize(input)
    @input = input
  end

  def rows
    @rows ||= input.split("\n").map { |row| row.split(' ').map(&:to_i) }
  end

  def columns
    @columns ||= rows.transpose
  end
end
