class Deque
  private

  attr_accessor :head, :tail

  class Node
    attr_reader :value
    attr_accessor :next, :prev

    def initialize(value)
      @value = value
    end
  end

  public

  def size
    count = 0
    node = head
    until node.nil?
      count += 1
      node = node.next
    end
    count
  end

  def push(value)
    node = Node.new(value)
    if tail.nil?
      self.head = node
    else
      node.prev = tail
      tail.next = node
    end
    self.tail = node
    self
  end

  def pop
    node = tail
    return if node.nil?

    if node.prev.nil?
      self.tail = self.head = nil
    else
      node.prev.next = nil
      self.tail = node.prev
    end
    node.value
  end

  def unshift(value)
    node = Node.new(value)
    if head.nil?
      self.tail = node
    else
      node.next = head
      head.prev = node
    end
    self.head = node
    self
  end

  def shift
    node = head
    return if node.nil?

    if node.next.nil?
      self.tail = self.head = nil
    else
      node.next.prev = nil
      self.head = node.next
    end
    node.value
  end

  def delete(value)
    node = head
    until node.nil?
      if node.value == value
        remove(node)
        break
      end
      node = node.next
    end
  end

  private

  def remove(node)
    if node.next.nil?
      self.tail = node.prev
    else
      node.next.prev = node.prev
    end

    if node.prev.nil?
      self.head = node.next
    else
      node.prev.next = node.next
    end
  end
end
