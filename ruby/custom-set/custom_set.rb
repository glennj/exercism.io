# OK, this passes all the tests, but c'mon...
#
# require 'set'
# class CustomSet < Set; end

class CustomSet
  def initialize(elements)
    @set = elements.zip([nil] * elements.length).to_h
  end

  def elements
    @set.keys.sort
  end

  def size
    @set.size
  end

  # methods that return a boolean

  def empty?
    @set.empty?
  end

  def member?(elem)
    @set.key? elem
  end

  def subset?(other)
    elements.none? { |e| !other.member?(e) }
  end

  def disjoint?(other)
    elements.none? { |e| other.member?(e) }
  end

  def ==(other)
    size == other.size && subset?(other)
  end

  # methods that return a CustomSet

  def add(elem)
    @set[elem] = nil
    self
  end

  def intersection(other)
    self.class.new(elements.select { |e| other.member?(e) })
  end

  def difference(other)
    self.class.new(elements.reject { |e| other.member?(e) })
  end

  def union(other)
    self.class.new(elements.concat(other.elements))
  end
end
