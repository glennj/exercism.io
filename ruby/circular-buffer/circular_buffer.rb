# An implementation using an array as a Stack:
# the read pointer is fixed at the head of the stack,
# the write pointer is fixed at the tail of the stack.

class CircularBuffer
  class BufferEmptyException < StandardError; end
  class BufferFullException  < StandardError; end

  def initialize(size)
    @size = size
    clear
  end

  def clear
    @buffer = []
  end

  def read
    raise BufferEmptyException if @buffer.empty?
    @buffer.shift
  end

  def write(data)
    raise BufferFullException if @buffer.length == @size
    @buffer.push data
  end

  def write!(data)
    read if @buffer.length == @size
    write(data)
  end
end

__END__

# A "proper" circular buffer implementation

class CircularBuffer
  class BufferEmptyException < StandardError; end
  class BufferFullException  < StandardError; end
  
  Pointer = Struct.new(:read, :write)

  def initialize(size)
    @size = size
    clear
  end

  def clear
    @buffer  = Array.new(@size)
    @pointer = Pointer.new(0, 0)
    self
  end

  def read
    data = @buffer[@pointer.read]
    raise BufferEmptyException if data.nil?

    @buffer[@pointer.read] = nil
    incr(:read)
    data
  end

  def write(data)
    raise BufferFullException if full?
    @buffer[@pointer.write] = data
    incr(:write)
    self
  end

  def write!(data)
    read if full?
    write(data)
    self
  end

  def full?
    @pointer.read == @pointer.write && !@buffer[@pointer.read].nil?
  end

  private

  def incr(what)
    raise "bad pointer #{what}" unless %i[read write].include? what
    getter = what
    setter = (what.to_s + '=').to_sym
    
    current_addr = @pointer.send(getter)
    next_addr = (current_addr + 1) % @buffer.length
    @pointer.send(setter, next_addr)
  end
end
