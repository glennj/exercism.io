class Bst
  attr_reader :left, :right, :data
  @left = nil
  @right = nil

  def initialize(value)
    @data = value
  end

  def insert(value)
    if value <= data
      if left
        left.insert value
      else
        @left = self.class.new value
      end
    else
      if right
        right.insert value
      else
        @right = self.class.new value
      end
    end
  end

  def each(&block)
    if block_given?
      left.each(&block) if left
      yield data
      right.each(&block) if right
    else
      to_enum :each
    end
  end
end
