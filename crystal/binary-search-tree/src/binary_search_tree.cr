class Node(T)
  include Enumerable(T)
  include Iterable(T)

  property value : T
  property left  : Node(T)?
  property right : Node(T)?
  property parent : Node(T)?

  def initialize(@value : T)
  end

  def insert(val)
    if val <= value
      if left.nil?
        @left = Node.new(val)
        left.try(&.parent = self)
      else
        left.try &.insert(val)
      end
    else
      if right.nil?
        @right = Node.new(val)
        right.try(&.parent = self)
      else
        right.try &.insert(val)
      end
    end
  end

  def search(val : T)
    case
    when val == value then self
    when val <= value then left.try &.search(val)
    when val  > value then right.try &.search(val)
    end
  end

  def delete(val : T)
    nodeToDelete = search(val)
    nodeToDelete.deleteMe(self) if !nodeToDelete.nil?
    self
  end

  def each
    NodeIterator.new(self)
  end

  def each(&block : T -> _)
    iterator = each
    iterator.each {|val| yield val}
  end

  private class NodeIterator(T)
    include Iterator(T)
    @stack = [] of Node(T)

    def initialize(node : Node(T))
      stack node
    end

    def stack(node)
      node.try do |n|
        @stack << n
        # add all the left descendants to the stack
        stack n.left
      end
    end

    def next
      if @stack.empty?
        stop
      else
        node = @stack.pop
        stack node.right
        node.value
      end
    end
  end

  # algorithm from https://en.wikipedia.org/wiki/Binary_search_tree#Deletion
  protected def deleteMe(tree_root : Node(T))
    if left.nil?
      shift_nodes tree_root, right
    elsif right.nil?
      shift_nodes tree_root, left
    else
      e = successor
      if e.try(&.parent) != self
        e.try(&.shift_nodes(tree_root, e.try(&.right)))
        e.try(&.right = right)
        e.try(&.right.try(&.parent = e))
      end
      shift_nodes tree_root, e
      e.try(&.left = left)
      e.try(&.left.try(&.parent = e))
    end
  end

  protected def successor
    node = right
    if !node.nil?
      until node.try(&.left).nil?
        node = node.try(&.left)
      end
    end
    node
  end

  protected def shift_nodes(tree_root, v)
    if parent.nil?
      tree_root.value = v.try(&.value) || 0
      tree_root.left = v.try(&.left)
      tree_root.right = v.try(&.right)
    elsif self == parent.try(&.left)
      parent.try(&.left = v)
    else
      parent.try(&.right = v)
    end
    if !v.nil?
      v.parent = parent
    end
  end
end
