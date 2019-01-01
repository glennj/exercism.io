class Deque
  class Node
    attr_reader :value
    attr_accessor :next, :prev

    def initialize(value)
      @value = value
    end
  end

  def size
    count = 0
    node = @head
    until node.nil?
      count += 1
      node = node.next
    end
    count
  end

  def push(value)
    node = Node.new(value)
    if @tail.nil?
      @head = node
    else
      node.prev = @tail
      @tail.next = node
    end
    @tail = node
    self
  end

  def pop
    node = @tail
    return if node.nil?

    if node.prev.nil?
      @tail = @head = nil
    else
      node.prev.next = nil
      @tail = node.prev
    end
    node.value
  end

  def unshift(value)
    node = Node.new(value)
    if @head.nil?
      @tail = node
    else
      node.next = @head
      @head.prev = node
    end
    @head = node
    self
  end

  def shift
    node = @head
    return if node.nil?

    if node.next.nil?
      @tail = @head = nil
    else
      node.next.prev = nil
      @head = node.next
    end
    node.value
  end

  def delete(value)
    node = @head
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
      @tail = node.prev
    else
      node.next.prev = node.prev
    end

    if node.prev.nil?
      @head = node.next
    else
      node.prev.next = node.next
    end
  end
end
