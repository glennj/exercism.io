import {Bucket} from './bucket'

const gcd = (a: number, b: number): number => (b ? gcd(b, a % b) : a)

export class TwoBucket {

  readonly goal: number
  private buckets: Bucket[]
  private isValid = false
  private errorMsg = ''
  private hasBeenRun = false
  private movesRequired = 0
  private goalName = ''
  private otherAmount = 0

  constructor(size1: number, size2: number, goal: number, startBucket: string) {
    this.goal = goal
    this.buckets = [new Bucket('one', size1), new Bucket('two', size2)]

    if (startBucket === 'two')
      this.buckets.reverse()

    /* This would be the logical place to throw.
     * But the tests demand otherwise.
     */
    this.isValid = this.validate()
  }

  get first(): Bucket { return this.buckets[0] }
  get second(): Bucket { return this.buckets[1] }

  private validate(): boolean {
    if (this.goal > Math.max(this.first.size, this.second.size)) {
      this.errorMsg = 'Goal is bigger than largest bucket.'
      return false
    }

    if (this.goal % gcd(this.first.size, this.second.size) !== 0) {
      this.errorMsg = 'Goal must be a multiple of the GCD of the sizes of the two buckets.'
      return false
    }

    return true
  }

  moves(): number {
    if (!this.isValid)
      throw new Error(this.errorMsg)

    if (!this.hasBeenRun) {
      [this.goalName, this.otherAmount, this.movesRequired] = this.calculateMoves()
      this.hasBeenRun = true
    }

    return this.movesRequired
  }

  get goalBucket(): string {
    if (!this.hasBeenRun)
      throw new Error('Run "moves()" first')

    return this.goalName
  }

  get otherBucket(): number {
    if (!this.hasBeenRun)
      throw new Error('Run "moves()" first')

    return this.otherAmount
  }

  private calculateMoves(): [string, number, number] {
    this.first.empty()
    this.second.empty()
    let moves = 0

    // move 1
    this.first.fill()
    moves++

    // move 2, perhaps
    if (this.second.size === this.goal) {
      this.second.fill()
      moves++
    }

    /* eslint-disable-next-line no-constant-condition */
    while (true) {
      if (this.first.amount === this.goal)
        return [this.first.name, this.second.amount, moves]

      if (this.second.amount === this.goal)
        return [this.second.name, this.first.amount, moves]

      if (this.first.isEmpty())
        this.first.fill()

      else if (this.second.isFull())
        this.second.empty()

      else
        this.first.pourInto(this.second)

      moves++
    }
  }
}
