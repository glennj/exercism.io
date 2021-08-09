var BufferFullError  = "Buffer full"
var BufferEmptyError = "Buffer empty"

class CircularBuffer {
  construct new(size) {
    _size = size
    _buffer = []
  }

  isFull  { _buffer.count == _size }
  isEmpty { _buffer.isEmpty }

  clear() { _buffer.clear() }

  write(elem) {
    if (isFull) Fiber.abort(BufferFullError)
    _buffer.add(elem)
  }

  read() {
    if (isEmpty) Fiber.abort(BufferEmptyError)
    return _buffer.removeAt(0)
  }

  forceWrite(elem) {
    if (isFull) read()
    write(elem)
  }
}
