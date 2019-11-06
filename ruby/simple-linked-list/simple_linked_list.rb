# comment
class Element
  attr_accessor :next, :datum

  def initialize(value)
    @datum = value
    @next = nil
  end
end

# comment
class SimpleLinkedList
  attr_reader :head
  private
  attr_writer :head

  public

  def initialize(array = [])
    @head = nil
    array.each { |value| push Element.new(value) }
  end

  # push and pop operate at the head of the list
  def push(node)
    node.next = head
    self.head = node
    self
  end

  def pop
    node = head
    self.head = node.next if node
    node
  end

  def each
    if block_given?
      node = head
      until node.nil?
        yield node
        node = node.next
      end
    else
      to_enum
    end
  end

  # collect a list of each node's datum
  def to_a
    each.map(&:datum)
  end

  def reverse!
    reversed = self.class.new
    reversed.push(pop) until head.nil?
    self.head = reversed.head
    self
  end
end
