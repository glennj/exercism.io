class Matrix
  @matrix : Array(Array(Int32))

  def initialize(input : String)
    @matrix = input.split("\n").map {|line| line.split.map(&.to_i) }
  end

  def row(n)
    @matrix[n - 1]
  end

  def column(n)
    @matrix.transpose[n - 1]
  end
end
