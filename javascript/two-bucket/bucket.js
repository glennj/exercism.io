export class Bucket {
  constructor(name, size) {
    this.name = name;
    this.size = size;
    this.amount = 0;
  }

  // accessors
  get available() { return this.size - this.amount; }
  get isFull()    { return this.amount === this.size; }
  get isEmpty()   { return this.amount === 0; }

  fill()  { this.amount = this.size; }
  empty() { this.amount = 0; }

  pourInto(other) {
    const quantity = Math.min(this.amount, other.available);
    this.amount  -= quantity;
    other.amount += quantity;
    // Law of Demeter, sure, but other is a bucket:
    // I already have total knowledge of its internals.
  }
}
