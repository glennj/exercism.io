class BinarySearch
  attr_reader :list

  def initialize(list)
    raise ArgumentError unless list == list.sort
    
    @list = list
  end

  def middle(i = 0, j = @list.length - 1)
    i + ((j - i) * 0.5).ceil
  end

  def search_for(elem, i = 0, j = @list.length - 1)
    raise "#{elem} not in list" if i > j

    mid = middle(i, j)

    return mid if elem == @list[mid]

    j = mid - 1 if elem < @list[mid]
    i = mid + 1 if elem > @list[mid]
    search_for elem, i, j
  end
end
