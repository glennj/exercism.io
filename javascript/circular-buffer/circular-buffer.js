export class BufferFullError extends Error {}
export class BufferEmptyError extends Error {}

class CircularBuffer {
  constructor(size) {
    this.size = size;
    this.clear();
  }

  clear() {
    this.buffer = new Array(this.size).fill(null);
    this.cursor = { read: 0, write: 0 };
    return this;
  }

  read() {
    const value = this.buffer[this.cursor.read];
    if (value === null) throw new BufferEmptyError();
    this.buffer[this.cursor.read] = null;
    this.incr('read');
    return value;
  }

  write(data, { force } = { force: false }) {
    if (data !== undefined && data !== null) {
      if (this.isFull() && !force) throw new BufferFullError();
      this.buffer[this.cursor.write] = data;
      this.incr('write');
    }
    return this;
  }

  forceWrite(data) {
    const full = this.isFull();
    this.write(data, { force: true });
    if (full) this.incr('read');
    return this;
  }

  isFull() {
    return this.cursor.read === this.cursor.write
        && this.buffer[this.cursor.read] !== null;
  }

  incr(c) {
    this.cursor[c] = (this.cursor[c] + 1) % this.size;
  }
}

export default function (size) {
  return new CircularBuffer(size);
}
