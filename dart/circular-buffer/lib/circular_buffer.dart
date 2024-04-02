/* These implementation definitions enable zero-argument contructors:
 *    throw EmptyBufferException();
 */

class EmptyBufferException implements Exception {}

class FullBufferException implements Exception {}

class CircularBuffer {
  int size;
  List<int> data = [];
  CircularBuffer(this.size);

  void clear() {
    this.data = [];
  }

  int read() {
    if (this.data.isEmpty) throw EmptyBufferException();
    return this.data.removeAt(0);
  }

  void write(int item, {bool force = false}) {
    if (this.data.length == this.size) {
      if (!force) throw FullBufferException();
      read();
    }
    this.data.add(item);
  }
}
