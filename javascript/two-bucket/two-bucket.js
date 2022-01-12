import { Bucket } from './bucket';

const gcd = (a, b) => b === 0 ? a : gcd(b, a % b);

export class TwoBucketError extends Error {}

export class TwoBucket {
  // private fields
  #goal;
  #buckets;
  #goalBucketName;
  #otherBucketAmount;
  #movesRequired;
  #isValid;
  #errorMsg;

  constructor(size1, size2, goal, start) {
    this.#goal = goal;
    this.#buckets = [new Bucket('one', size1), new Bucket('two', size2)];

    if (start === 'two') {
      this.#buckets.reverse();
    }

    // tests require that we delay throwing exceptions until
    // the move() function is called.
    this.#isValid = this.#validate();
  }

  get goal()   { return this.#goal; }
  get first()  { return this.#buckets[0]; }
  get second() { return this.#buckets[1]; }

  #validate() {
    if (this.goal > Math.max(this.first.size, this.second.size)) {
      this.#errorMsg = 'Goal is bigger than the largest bucket.';
      return false
    }

    if (this.goal % gcd(this.first.size, this.second.size) !== 0) {
      this.#errorMsg = 'Goal must be a multiple of the GCD of the sizes of the two buckets.';
      return false;
    }

    return true;
  }

  // -----------------------------------------------------------
  moves() {
    if (!this.#isValid) {
      throw new TwoBucketError(this.#errorMsg);
    }

    if (this.#movesRequired === undefined) {
      [this.#movesRequired, this.#goalBucketName, this.#otherBucketAmount] = this.#calculateMoves();
    }

    return this.#movesRequired;
  }

  get goalBucket() { 
    if (this.#goalBucketName === undefined) {
      throw new TwoBucketError('Run "moves()" first');
    }

    return this.#goalBucketName;
  }

  get otherBucket() { 
    if (this.#otherBucketAmount === undefined) {
      throw new TwoBucketError('Run "moves()" first');
    }

    return this.#otherBucketAmount;
  }

  // -----------------------------------------------------------
  #calculateMoves() {
    this.first.empty();
    this.second.empty();
    let moves = 0;

    // fill the start bucket with the first move
    this.first.fill();
    moves += 1;

    // optimization: if the other bucket is the right size,
    // fill it immediately with the second move
    if (this.second.size === this.goal) {
      this.second.fill();
      moves += 1;
    }

    /* eslint-disable-next-line no-constant-condition */
    while (true) {
      if (this.first.amount === this.goal) {
        return [moves, this.first.name, this.second.amount];
      }

      if (this.second.amount === this.goal) {
        return [moves, this.second.name, this.first.amount];
      }

      if (this.first.isEmpty) {
        this.first.fill();
      }
      else if (this.second.isFull) {
        this.second.empty();
      }
      else {
        this.first.pourInto(this.second);
      }

      moves += 1;
    }
  }
}
