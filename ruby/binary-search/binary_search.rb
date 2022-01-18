class BinarySearch
  attr_reader :list

  def initialize(list)
    raise ArgumentError unless list == list.sort

    @list = list
  end

  def middle(left = 0, right = @list.length - 1)
    (left + right) / 2
  end

  def search_for(elem, left = 0, right = @list.length - 1)
    return nil if left > right

    mid = middle(left, right)

    return mid if elem == @list[mid]

    right = mid - 1 if elem < @list[mid]
    left  = mid + 1 if elem > @list[mid]
    search_for elem, left, right
  end
end
