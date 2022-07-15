import "./circular-buffer" for CircularBuffer, BufferEmptyError, BufferFullError
import "wren-testie/testie" for Testie, Expect

Testie.test("CircularBuffer") { |do, skip|
  do.test("reading empty buffer should fail") {
    var buffer = CircularBuffer.new(1)
    Expect.that { buffer.read() }.abortsWith(BufferEmptyError)
  }

  skip.test("can read an item just written") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    Expect.value(buffer.read()).toBe("1")
  }

  skip.test("each item may only be read once") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    Expect.value(buffer.read()).toBe("1")
    Expect.that { buffer.read() }.abortsWith(BufferEmptyError)
  }

  skip.test("items are read in the order they are written") {
    var buffer = CircularBuffer.new(2)
    buffer.write("1")
    buffer.write("2")
    Expect.value(buffer.read()).toBe("1")
    Expect.value(buffer.read()).toBe("2")
  }

  skip.test("full buffer can't be written to") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    Expect.that { buffer.write(2) }.abortsWith(BufferFullError)
  }

  skip.test("a read frees up capacity for another write") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    Expect.value(buffer.read()).toBe("1")
    buffer.write("2")
    Expect.value(buffer.read()).toBe("2")
  }

  skip.test("read position is maintained even across multiple writes") {
    var buffer = CircularBuffer.new(3)
    buffer.write("1")
    buffer.write("2")
    Expect.value(buffer.read()).toBe("1")
    buffer.write("3")
    Expect.value(buffer.read()).toBe("2")
    Expect.value(buffer.read()).toBe("3")
  }

  skip.test("items cleared out of buffer can't be read") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    buffer.clear()
    Expect.that { buffer.read() }.abortsWith(BufferEmptyError)
  }

  skip.test("clear frees up capacity for another write") {
    var buffer = CircularBuffer.new(1)
    buffer.write("1")
    buffer.clear()
    buffer.write("2")
    Expect.value(buffer.read()).toBe("2")
  }

  skip.test("clear does nothing on empty buffer") {
    var buffer = CircularBuffer.new(1)
    buffer.clear()
    buffer.write("1")
    Expect.value(buffer.read()).toBe("1")
  }

  skip.test("forceWrite acts like write on non-full buffer") {
    var buffer = CircularBuffer.new(2)
    buffer.write("1")
    buffer.forceWrite("2")
    Expect.value(buffer.read()).toBe("1")
    Expect.value(buffer.read()).toBe("2")
  }

  skip.test("forceWrite replaces the oldest item on full buffer") {
    var buffer = CircularBuffer.new(2)
    buffer.write("1")
    buffer.write("2")
    buffer.forceWrite("3")
    Expect.value(buffer.read()).toBe("2")
    Expect.value(buffer.read()).toBe("3")
  }

  skip.test("forceWrite replaces the oldest item remaining in buffer following a read") {
    var buffer = CircularBuffer.new(3)
    buffer.write("1")
    buffer.write("2")
    buffer.write("3")
    Expect.value(buffer.read()).toBe("1")
    buffer.write("4")
    buffer.forceWrite("5")
    Expect.value(buffer.read()).toBe("3")
    Expect.value(buffer.read()).toBe("4")
    Expect.value(buffer.read()).toBe("5")
  }

  skip.test("initial clear does not affect wrapping around") {
    var buffer = CircularBuffer.new(2)
    buffer.clear()
    buffer.write("1")
    buffer.write("2")
    buffer.forceWrite("3")
    buffer.forceWrite("4")
    Expect.value(buffer.read()).toBe("3")
    Expect.value(buffer.read()).toBe("4")
    Expect.that { buffer.read() }.abortsWith(BufferEmptyError)
  }
}
