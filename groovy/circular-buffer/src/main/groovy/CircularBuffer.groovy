class EmptyBufferException extends Exception {}
class FullBufferException extends Exception {}

class CircularBuffer {
    def capacity
    def data
    def count
    def readIdx
    def writeIdx

    CircularBuffer(int capacity) {
        this.data = new ArrayList(capacity)
        this.capacity = capacity
        this.clear()
    }

    def clear() {
        this.count = 0
        this.readIdx = 0
        this.writeIdx = 0
    }

    def read() {
        if (this.count == 0) {
            throw new EmptyBufferException()
        }
        def item = this.data[this.readIdx]
        this.count--
        this.readIdx = (this.readIdx + 1) % this.capacity
        return item
    }

    def write(int item) {
        this._do_write(item, false)
    }

    def overwrite(int item) {
        this._do_write(item, true)
    }

    def _do_write(int item, boolean overwrite) {
        if (this.count == this.capacity) {
            if (overwrite) {
                this.read()
            }
            else {
                throw new FullBufferException()
            }
        }
        this.data[this.writeIdx] = item
        this.count++
        this.writeIdx = (this.writeIdx + 1) % this.capacity
    }
}
