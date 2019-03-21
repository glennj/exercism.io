# Thanks to @mengqing for the inspiration.

############################################################
class Node
  def initialize(value, left, right)
    @value = value
    @left  = left
    @right = right
  end
  attr_accessor :value, :left, :right

  def ==(other)
    other.class == self.class && other.state == self.state
  end

  protected def state
    [self.value, self.left, self.right]
  end
end
    
############################################################
class Zipper
  def self.from_tree(tree)
    new(tree)
  end

  def initialize(tree, path = [])
    @tree = tree
    @path = path
  end
  attr_reader :tree

  def to_tree
    @path.first&.tree || @tree
  end

  def left
    if not @tree.left.nil?
      self.class.new(@tree.left, @path + [self])
    end
  end

  def right
    if not @tree.right.nil?
      self.class.new(@tree.right, @path + [self])
    end
  end

  def value
    @tree.value
  end

  def up
    if not @path.empty?
      previous = @path.pop
      self.class.new(previous.tree, @path)
    end
  end

  def set_value(value)
    @tree.value = value
    self
  end

  def set_left(tree)
    @tree.left = tree
    self
  end

  def set_right(tree)
    @tree.right = tree
    self
  end

  def ==(other)
    other.class == self.class && other.state == self.state
  end

  protected def state
    [self.tree]
  end
end
