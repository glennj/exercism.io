class Bst
  attr_reader :left, :right, :data

  private

  attr_writer :left, :right

  public

  def initialize(value)
    @data = value
    @left = nil
    @right = nil
  end

  def insert(value)
    if value <= data
      if left
        left.insert value
      else
        self.left = self.class.new value
      end
    else
      if right
        right.insert value
      else
        self.right = self.class.new value
      end
    end
  end

  def each(&block)
    if block_given?
      left&.each(&block)
      yield data
      right&.each(&block)
    else
      to_enum :each
    end
  end
end
