# monkeypatching Array class
class Array
  def keep
    if block_given?
      result = []
      each { |elem| result << elem if yield(elem) }
      result
    else
      to_enum
    end
  end

  def discard
    if block_given?
      keep { |elem| !yield(elem) }
    else
      to_enum
    end
  end
end
