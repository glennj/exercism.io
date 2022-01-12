export class Bucket {
  readonly size: number
  readonly name: string
  private currentAmount = 0

  constructor(name: string, size: number) {
    this.name = name
    this.size = size
  }

  get amount(): number { return this.currentAmount }
  get available(): number { return this.size - this.amount }

  isFull(): boolean { return this.amount === this.size }
  isEmpty(): boolean { return this.amount === 0 }

  fill(): void { this.currentAmount = this.size }
  empty(): void { this.currentAmount = 0 }

  pourInto(other: Bucket): void {
    const quantity = Math.min(this.amount, other.available)
    this.currentAmount -= quantity
    other.currentAmount += quantity
  }
}
