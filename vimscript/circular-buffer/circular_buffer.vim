"
" Returns a dictionary representing a circular buffer with a set capacity
"
function! CircularBuffer(capacity) abort
  let buffer = {}
  let buffer.capacity = a:capacity
  let buffer.data = [' ']->repeat(a:capacity)

  function buffer.Clear() abort
    let self.count = 0
    let self.readIdx = 0
    let self.writeIdx = 0
  endfunction

  call buffer.Clear()

  function buffer.incr(pointer) abort
    let self[a:pointer] = (self[a:pointer] + 1) % self.capacity
  endfunction

  function buffer.Read() abort
    if self.count == 0
      throw "Empty buffer"
    endif
    let value = self.data[self.readIdx]
    call self.incr("readIdx")
    let self.count -= 1
    return value
  endfunction

  function buffer.Write(item) abort
    if self.count == self.capacity
      throw "Full buffer"
    endif
    let self.data[self.writeIdx] = a:item
    call self.incr("writeIdx")
    let self.count += 1
  endfunction

  function buffer.Overwrite(item) abort
    if self.count == self.capacity
      call self.Read()
    endif
    call self.Write(a:item)
  endfunction

  return buffer
endfunction
