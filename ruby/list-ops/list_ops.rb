# What built-in list operations are so fundamental that we need them
# to build the rest?
#
# For example, I'd like to implement "array size" with something
# simple like: "count until nil?" but that automatically precludes
# counting any unitialized array or any array with nil as an element.
#
# I've used the following builtin Array instance methods:
#   #each_index
#   #[]
#   #<<
#   #delete_at
#
# and class methods ::[] to instantiate an array.

class ListOps
  # Here, I'm stealing ruby's :each_index so I can iterate over
  # an array of equal size where we know there are no nil elements
  def self.arrays(array)
    size = 0
    array.each_index { size += 1 }
    size
  end

  ###############################################
  # These work, but the performace is particularly attrocious.
  # I'm using builtin method :<< thoughout
  def self.pusher(array, element)
    s = arrays(array)
    array[s] = element
    array
  end

  def self.popper(array)
    s = arrays(array)
    return if s.zero?

    result = array[s - 1]
    # Another fundamental ruby array method.
    # We could return a slice, but we do want to
    # mutate the given array.
    array.delete_at(s - 1)
    result
  end

  def self.unshifter(array, element)
    concatter([element], array)
  end

  def self.shifter(array)
    # have to count the array, even though
    # we're only dealing with the first element.
    s = arrays(array)
    return if s.zero?

    result = array[0]
    array.delete_at(0)
    result
  end
  ###############################################

  def self.mapper(array)
    result = []
    if block_given?
      s = arrays(array)
      (0...s).each do |i|
        result << yield(array[i])
      end
    else
      result = concatter(array)
    end
    result
  end

  def self.each_thingy(array)
    if block_given?
      s = arrays(array)
      (0...s).each do |i|
        yield(array[i])
      end
    end
    array
  end

  def self.reverser(array)
    reversed = []
    (arrays(array) - 1).downto(0) do |i|
      reversed << array[i]
    end
    reversed
  end

  def self.concatter(*arrays_to_concat)
    result = []
    each_thingy(arrays_to_concat) do |array|
      each_thingy(array) do |element|
        result << element
      end
    end
    result
  end

  def self.filterer(array)
    result = []
    if block_given?
      each_thingy(array) do |element|
        result << element if yield(element)
      end
    else
      result = concatter(array)
    end
    result
  end

  # should also work for strings and other ducks that can :+
  # e.g. sum_reducer(%w[w o r l d], 'hello') == 'helloworld'
  def self.sum_reducer(array, sum = 0)
    each_thingy(array) do |thing|
      sum += thing
    end
    sum
  end

  # should also work for strings and other ducks that can :*
  # e.g. factorial_reducer([1,2,3], '*') == '*' * 6 == '******'
  def self.factorial_reducer(array, product = 1)
    each_thingy(array) do |num|
      product *= num
    end
    product
  end
end
