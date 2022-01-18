class Array
  def accumulate
    if block_given?
      result = []
      each { |elem| result << yield(elem) }
      result
    else
      to_enum
    end
  end
end
