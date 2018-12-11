export class Bucket {
  size: number
  amount: number = 0
  readonly name: string

  constructor(name: string) { this.name = name }

  private static _one: Bucket
  static get One() { return this._one || (this._one = new this('one')) }

  private static _two: Bucket
  static get Two() { return this._two || (this._two = new this('two')) }

  get capacity(): number { return this.size - this.amount }
  fill(): void { this.amount = this.size }
  empty(): void { this.amount = 0 }
  isFull(): boolean { return this.amount === this.size }
  isEmpty(): boolean { return this.amount === 0 }

  pour(other: Bucket): void {
    const toPour = Math.min(this.amount, other.capacity)
    this.amount -= toPour
    other.amount += toPour
  }
}

const GCD = (a: number, b: number): number => (b ? GCD(b, a % b) : a)

export class TwoBucket {
  readonly one: Bucket = Bucket.One
  readonly two: Bucket = Bucket.Two

  private _moves: number = 0
  goalBucket: string
  otherBucket: number = 0

  constructor(size1: number, size2: number, goal: number, startBucket: Bucket) {
    if (goal > Math.max(size1, size2)) {
      throw new Error('No solution: goal bigger than largest bucket')
    }

    const gcd = GCD(size1, size2)
    if (gcd !== 1 && goal % gcd !== 0) {
      // buckets are not relatively prime. Goal must be a multiple of the gcd
      throw new Error('No solution possible')
    }

    this.one.size = size1
    this.two.size = size2
    const other = startBucket.name === 'one' ? this.two : this.one;
    [this.goalBucket, this.otherBucket, this._moves] = this.solve(startBucket, other, goal)
  }

  moves(): number { return this._moves }

  private solve(start: Bucket, other: Bucket, goal: number): [string, number, number] {
    start.empty()
    other.empty()
    let moves = 0

    while (true) {
      if (start.amount === goal) {
        return [start.name, other.amount, moves]
      }
      if (other.amount === goal) {
        return [other.name, start.amount, moves]
      }

      if (start.isEmpty()) {
        start.fill()
      }
      else if (other.isFull()) {
        other.empty()
      }
      else {
        start.pour(other)
      }
      moves++
    }
  }
}
