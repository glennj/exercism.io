import scala.collection.mutable.ArraySeq

class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(val capacity: Int) {

  private val array: ArraySeq[Int] = Array.ofDim[Int](capacity)
  private var readp = 0
  private var writep = 0
  private var count = 0

  def isFull: Boolean = count == capacity
  def isEmpty: Boolean = count == 0

  def write(value: Int) = 
    if isFull then throw FullBufferException()
    array.update(writep, value)
    writep = (writep + 1) % capacity
    count = count + 1

  def read(): Int = 
    if isEmpty then throw EmptyBufferException()
    val value = array.apply(readp)
    readp = (readp + 1) % capacity
    count = count - 1
    value

  def overwrite(value: Int) = 
    if isFull then read()
    write(value)

  def clear() =
    count = 0
    writep = readp
}
