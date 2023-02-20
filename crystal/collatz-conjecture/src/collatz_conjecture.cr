module CollatzConjecture
  def self.steps(input : Int32, count = 0) : Int32
    raise ArgumentError.new if input <= 0
    case
      when input == 1 then count
      when input.even? then steps(input // 2, count + 1)
      else steps(3 * input + 1, count + 1)
    end
  end
end
