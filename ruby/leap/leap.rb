module Year
  module_function

  def leap?(year)
    (year % 400).zero? || ((year % 4).zero? && (year % 100).nonzero?)
  end
end
