module CollatzConjecture
  module_function

  # tailcall recursive
  # def steps(num, step = 0)
  #   raise ArgumentError if num <= 0
  #
  #   return step if num == 1
  #   num = num.even? ? (num / 2) : (3 * num + 1)
  #   steps(num, step + 1)
  # end

  # iterative
  def steps(num)
    raise ArgumentError if num <= 0

    step = 0
    until num == 1
      num = num.even? ? (num / 2) : (3 * num + 1)
      step += 1
    end
    step
  end
end
