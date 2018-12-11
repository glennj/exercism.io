export class BufferOverflowError extends Error {}
export class BufferEmptyError    extends Error {}

export default class CircularBuffer<T> {
  private size: number
  private buffer: Array<T | undefined>
  private pointer: { [index: string]: number }

  constructor(size: number) {
    this.size = size
    this.clear()
  }

  clear(): CircularBuffer<T> {
    this.buffer = new Array<T | undefined>(this.size)
    this.pointer = {read: 0, write: 0}
    return this
  }

  read(): T {
    const value = this.buffer[this.pointer.read]
    if (value === undefined) { throw new BufferEmptyError() }
    this.buffer[this.pointer.read] = undefined
    this.incr('read')
    return value
  }

  write(value: T, force: boolean = false): void {
    if (this.isFull() && !force) { throw new BufferOverflowError() }
    this.buffer[this.pointer.write] = value
    this.incr('write')
  }

  forceWrite(value: T): void {
    if (this.isFull()) { this.incr('read') }
    this.write(value, true)
  }

  private isFull(): boolean {
    return this.pointer.read === this.pointer.write
        && this.buffer[this.pointer.read] !== undefined
  }

  private incr(cursor: 'read' | 'write'): void {
    this.pointer[cursor] += 1
    this.pointer[cursor] %= this.size
  }
}
