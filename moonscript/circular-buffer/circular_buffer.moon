-- This uses a fixed-size array, and read and write "heads" to navigate around the array.

class CircularBuffer
  new: (size) =>
    @buff = [0 for _ = 1, size]
    @size = size
    @count = 0
    @head = {r: 0, w: 0}

  is_empty: => @count == 0
  is_full:  => @count == @size

  read: =>
    if @is_empty!
      return nil, false
    item = @buff[@head.r]
    @head.r = (@head.r == @size) and 1 or @head.r + 1
    @count -= 1
    item, true

  write: (item, opts={}) =>
    if @is_full!
      if opts.overwrite
        @read!
      else
        return false
    @buff[@head.w] = item
    @head.w = (@head.w == @size) and 1 or @head.w + 1
    @count += 1
    true

  clear: =>
    @head.r = @head.w
    @count = 0


CircularBuffer
